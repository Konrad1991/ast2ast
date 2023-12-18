printClass <- R6::R6Class("printClass",
	public = list(
		getAST = function(code) {
			if(!is.call(code)) {
				return(code)
			}
			code <- as.list(code)
			lapply(code, self$getAST)
		},

		getCALLS = function(code) {
			for(i in seq_along(1:length(code))) {
				if(is.list(code[[i]])) {
					code[[i]] <- self$getCALLS(code[[i]])
				}
			}
			code <- as.call(code)
			return(code)
		},

		indentationLevel = 0,

		printAST = function(code) {
			for(i in seq_along(1:length(code))) {
				if(is.list(code[[i]])) {
					self$indentationLevel <- self$indentationLevel + 1
					fct <- deparse(code[[i]][[1]]); times <- rep("...", self$indentationLevel)
					print(paste(c(times, fct), collapse = ""))
					self$indentationLevel <- self$indentationLevel + 1
					self$printAST(code[[i]][2:length(code[[i]])])
					self$indentationLevel <- self$indentationLevel - 2
				} else {
					obj <- deparse(code[[i]]); times <- paste(rep("...", self$indentationLevel), collapse = "")
					print(paste(c(times, obj), collapse = " "), collapse = "")
				}
			}
		}

	)
)

#pC <- printClass$new()
#ast <- pC$getAST(quote(a*b + c*d/h -sin(f)) )
#pC$getCALLS(ast)
#pC$printAST(ast)
