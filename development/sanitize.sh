#!/usr/bin/env bash
#
# Run the ast2ast test suite under a sanitizer, using gcc (matches CRAN's
# gcc-UBSAN / gcc-ASAN flavours). clang is deliberately not used: it omits the
# UBSan runtime from -shared links, so the .so fails to load in R with
# undefined __ubsan_* symbols. gcc links libubsan/libasan, which are on the
# standard loader path, so a dlopen'd .so resolves cleanly.
#
#   development/sanitize.sh              # UBSan (default)
#   development/sanitize.sh ubsan
#   development/sanitize.sh asan         # ASan + UBSan, via LD_PRELOAD
#
# Env toggles:
#   SKIP_INSTALL=1   re-run the tests without recompiling the package
#   RECOVER=0        abort on the first sanitizer error (default: keep going)
#
# Run from anywhere; the script cd's to the package root itself. Nothing under
# ~/.R is touched -- flags are injected through R_MAKEVARS_USER, which both
# `R CMD INSTALL` and Rcpp::sourceCpp() (the compiler `translate()` shells out
# to) honour, so the package .so *and* the generated code are instrumented.

set -u

MODE="${1:-ubsan}"
PKG_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PKG_ROOT" || exit 1

LOG="$PKG_ROOT/development/sanitize-${MODE}.log"
MK="$(mktemp "${TMPDIR:-/tmp}/ast2ast-makevars.XXXXXX")"
DRIVER="$(mktemp "${TMPDIR:-/tmp}/ast2ast-driver.XXXXXX").R"
trap 'rm -f "$MK" "$DRIVER"' EXIT

# Run every tinytest file directly (not test_package), forcing at_home per file:
# test_package defaults at_home=FALSE and unsets TT_AT_HOME after each file, so
# the gated AD / codegen tests -- the ones a sanitizer most needs to see -- get
# skipped. Here we set it fresh before each run_test_file and pass at_home=TRUE.
cat > "$DRIVER" <<'EOF'
pkg  <- "ast2ast"
dir  <- system.file("tinytest", package = pkg)
files <- list.files(dir, pattern = "^test.*\\.R$", full.names = TRUE)
library(pkg, character.only = TRUE)
fail <- 0L
for (f in files) {
  Sys.setenv(TT_AT_HOME = "TRUE")
  res  <- tinytest::run_test_file(f, at_home = TRUE, verbose = 1)
  print(res)
  fail <- fail + sum(vapply(res, isFALSE, logical(1)))
}
cat(sprintf("\n== total failing tests: %d ==\n", fail))
if (fail > 0L) quit(status = 1L, save = "no")
EOF

RECOVER="${RECOVER:-1}"
if [ "$RECOVER" = "0" ]; then
  NORECOVER="-fno-sanitize-recover=all"
else
  NORECOVER=""
fi

# vptr: needs RTTI on every polymorphic type, noisy in a dlopen'd lib; CRAN's
# gcc-UBSAN excludes it too. (gcc's -fsanitize=function is 14+ and not in the
# `undefined` umbrella here, so nothing to exclude for it.)
NOSAN="-fno-sanitize=vptr"

case "$MODE" in
  ubsan)
    SAN="-fsanitize=undefined $NOSAN"
    PRELOAD=""
    ;;
  asan)
    SAN="-fsanitize=address,undefined $NOSAN"
    PRELOAD="$(ldconfig -p | awk '/libasan\.so/{print $NF; exit}')"
    if [ -z "$PRELOAD" ] || [ ! -e "$PRELOAD" ]; then
      echo "!! could not locate libasan.so -- install libasan (gcc runtime)" >&2
      exit 1
    fi
    ;;
  *)
    echo "usage: $(basename "$0") [ubsan|asan]" >&2
    exit 2
    ;;
esac

FLAGS="-O1 -g -fno-omit-frame-pointer $SAN $NORECOVER"

cat > "$MK" <<EOF
CXX        = g++
CXX14      = g++
CXX17      = g++
CXX20      = g++
CXXFLAGS   = $FLAGS
CXX14FLAGS = $FLAGS
CXX17FLAGS = $FLAGS
CXX20FLAGS = $FLAGS
LDFLAGS    = $SAN
EOF

export R_MAKEVARS_USER="$MK"
export TT_AT_HOME=TRUE
export UBSAN_OPTIONS="print_stacktrace=1:halt_on_error=$([ "$RECOVER" = 0 ] && echo 1 || echo 0)"
export ASAN_OPTIONS="detect_leaks=0:abort_on_error=$([ "$RECOVER" = 0 ] && echo 1 || echo 0):handle_abort=1"

echo "== ast2ast sanitizer run =============================================="
echo "  mode      : $MODE"
echo "  recover   : $RECOVER  (RECOVER=0 to abort on first hit)"
echo "  makevars  : $MK"
echo "  log       : $LOG"
[ -n "$PRELOAD" ] && echo "  preload   : $PRELOAD"
echo "----------------------------------------------------------------------"
sed 's/^/  | /' "$MK"
echo "======================================================================"

: > "$LOG"

if [ "${SKIP_INSTALL:-0}" != "1" ]; then
  echo ">> R CMD INSTALL (instrumented) ..."
  R CMD INSTALL --preclean --no-multiarch --no-test-load . 2>&1 | tee -a "$LOG"
  ins=${PIPESTATUS[0]}
  if [ "$ins" -ne 0 ]; then
    echo "!! install failed (exit $ins) -- see $LOG" >&2
    exit "$ins"
  fi
fi

echo ">> running every tinytest file with at_home=TRUE ..."
if [ -n "$PRELOAD" ]; then
  LD_PRELOAD="$PRELOAD" Rscript "$DRIVER" 2>&1 | tee -a "$LOG"
else
  Rscript "$DRIVER" 2>&1 | tee -a "$LOG"
fi
run=${PIPESTATUS[0]}

echo
echo "== sanitizer hits ===================================================="
if grep -nE "runtime error:|ERROR: (Address|LeakSanitizer)|(Address|Undefined|Leak)Sanitizer|SUMMARY: .*Sanitizer|heap-buffer-overflow|stack-buffer-overflow|use-after-(free|scope|return)|-buffer-overflow" "$LOG"; then
  echo "----------------------------------------------------------------------"
  echo "STATUS: sanitizer reported issues -- see $LOG"
  exit 1
else
  echo "  none"
  echo "----------------------------------------------------------------------"
  if [ "$run" -ne 0 ]; then
    echo "STATUS: no sanitizer hits, but tinytest exited $run (test failure) -- see $LOG"
    exit "$run"
  fi
  echo "STATUS: clean"
  exit 0
fi
