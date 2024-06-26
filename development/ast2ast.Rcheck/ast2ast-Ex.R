pkgname <- "ast2ast"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "ast2ast-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('ast2ast')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("J")
### * J

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: J
### Title: Calculates the jacobian function and translatesthe resulting
###   function into a C++ function.
### Aliases: J

### ** Examples
 # Further examples can be found in the vignettes. 
  ## Not run: 
##D       # simple example
##D       f <- function(y) {
##D         ydot <- vector(length = 2)
##D         a <- 1.1
##D         b <- 0.4
##D         c <- 0.1
##D         d <- 0.4
##D         ydot[1] <- y[1]*a - y[1]*y[2]*b
##D         ydot[2] <- y[2]*y[1]*c - y[2]*d
##D         return(ydot)
##D       }
##D       jac <- ast2ast::J(f, ydot, y, verbose = TRUE)
##D       jac(c(10, 11))
##D   
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("J", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("translate")
### * translate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: translate
### Title: Translates an R function into a C++ function.
### Aliases: translate

### ** Examples
 # Further examples can be found in the vignettes. 
    ## Not run: 
##D       # Hello World
##D       # ==========================================================================
##D       
##D       # Translating to R_fct
##D       # --------------------------------------------------------------------------
##D       f <- function() { print("Hello World!")}
##D       ast2ast::translate(f)
##D       f()
##D       
##D       # Translating to external pointer
##D       # --------------------------------------------------------------------------
##D       f <- function() { print("Hello World!")}
##D       pointer_to_f_cpp <- ast2ast::translate(f, 
##D                                              output = "XPtr", return_type = "void")
##D       Rcpp::sourceCpp(code = '
##D       #include <Rcpp.h>
##D       typedef void (*fp)();
##D 
##D       // [[Rcpp::export]]
##D       void call_fct(Rcpp::XPtr<fp> inp) {
##D         fp f = *inp;
##D         f(); } ')
##D         
##D       call_fct(pointer_to_f_cpp)
##D       
##D       # Run sum example:
##D       # ==========================================================================
##D       
##D       # R version of run sum
##D       # --------------------------------------------------------------------------
##D       run_sum <- function(x, n) {
##D         sz <- length(x)
##D         
##D         ov <- vector(mode = "numeric", length = sz)
##D         
##D         ov[n] <- sum(x[1:n])
##D         for(i in (n+1):sz) {
##D           
##D           ov[i] <- ov[i-1] + x[i] - x[i-n]
##D         }
##D         
##D         ov[1:(n-1)] <- NA
##D         
##D         return(ov)
##D       }
##D       
##D       # translated Version of R function
##D       # --------------------------------------------------------------------------
##D       run_sum_fast <- function(x, n) {
##D         sz <- length(x)
##D         ov <- vector(sz)
##D         
##D         sum_db = 0
##D         for(i in 1:n) {
##D           sum_db <- sum_db + at(x, i)
##D         }
##D         ov[n] <- sum_db
##D         
##D         for(i in (n + 1):sz) {
##D           ov[i] <- at(ov, i - 1) + at(x, i) - at(x, i - at(n, 1))
##D         }
##D         
##D         ov[1:(n - 1)] <- NA
##D         
##D         return(ov)
##D       }
##D       run_sum_cpp <- ast2ast::translate(run_sum_fast, verbose = FALSE)
##D       set.seed(42)
##D       x <- rnorm(10000)
##D       n <- 500
##D       one <- run_sum(x, n)
##D       two <- run_sum_cpp(x, n)
##D     
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("translate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
