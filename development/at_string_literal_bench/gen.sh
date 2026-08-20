#!/usr/bin/env bash
# Compares compile time / object size / stripped-binary size between calling
# etr::at() N times (all sharing one instantiation, today's behaviour) and
# calling the at_tagged<Expr>() variant from issue_variable_names.md N times
# with N distinct Expr strings (forcing N distinct instantiations).
set -euo pipefail
cd "$(dirname "$0")"

N="${1:?usage: gen.sh N}"
INCLUDE_DIR="../../inst/include"
# R's actual default CXX20FLAGS (from `R CMD config CXX20FLAGS`), plus -std=c++20.
CXXFLAGS="-std=c++20 -g -O2 -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer -fstack-protector-strong -fstack-clash-protection -Wformat -D_FORTIFY_SOURCE=3"

gen_baseline() {
  {
    echo '#define STANDALONE_ETR'
    echo "#include \"$INCLUDE_DIR/etr_bits/Core.hpp\""
    echo "#include \"$INCLUDE_DIR/etr_bits/Subsetting.hpp\""
    echo 'using namespace etr;'
    echo 'int main() {'
    echo "  Array<Double, Buffer<Double>> a(SI{std::size_t($N + 1)});"
    echo '  for (std::size_t i = 0; i <= (std::size_t)'"$N"'; i++) a.d.set(i, Double((double)i));'
    echo '  Double acc(0.0);'
    for i in $(seq 1 "$N"); do
      echo "  acc = acc + at(a, Integer($i));"
    done
    echo '  std::cout << get_val(acc) << std::endl;'
    echo '}'
  } > "baseline_${N}.cpp"
}

gen_tagged() {
  {
    echo '#define STANDALONE_ETR'
    echo "#include \"$INCLUDE_DIR/etr_bits/Core.hpp\""
    echo "#include \"$INCLUDE_DIR/etr_bits/Subsetting.hpp\""
    echo '#include "at_tagged.hpp"'
    echo 'using namespace etr;'
    echo 'int main() {'
    echo "  Array<Double, Buffer<Double>> a(SI{std::size_t($N + 1)});"
    echo '  for (std::size_t i = 0; i <= (std::size_t)'"$N"'; i++) a.d.set(i, Double((double)i));'
    echo '  Double acc(0.0);'
    for i in $(seq 1 "$N"); do
      echo "  acc = acc + at_tagged<\"a[$i]\">(a, Integer($i));"
    done
    echo '  std::cout << get_val(acc) << std::endl;'
    echo '}'
  } > "tagged_${N}.cpp"
}

report() {
  local label="$1" src="$2"
  local obj="${src%.cpp}.o"
  local exe="${src%.cpp}.exe"
  local t0 t1

  t0=$(date +%s.%N)
  g++ $CXXFLAGS -I "$INCLUDE_DIR" -c "$src" -o "$obj"
  t1=$(date +%s.%N)
  local compile_time
  compile_time=$(echo "$t1 - $t0" | bc)

  g++ $CXXFLAGS "$obj" -o "$exe"
  strip -o "${exe}.stripped" "$exe"

  local obj_size exe_size stripped_size
  obj_size=$(stat -c%s "$obj")
  exe_size=$(stat -c%s "$exe")
  stripped_size=$(stat -c%s "${exe}.stripped")

  printf "%-10s  N=%-5s  compile=%6.2fs  obj=%8d B  exe=%9d B  stripped=%8d B\n" \
    "$label" "$N" "$compile_time" "$obj_size" "$exe_size" "$stripped_size"
}

gen_baseline
gen_tagged
report "baseline" "baseline_${N}.cpp"
report "tagged" "tagged_${N}.cpp"
