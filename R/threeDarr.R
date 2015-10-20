"threeDarr" <-
function (..., rep=1, union=TRUE, slicenames=NULL) 
{
	fun.copyright <- "Placed in the public domain 2011-2014 by Burns Statistics Ltd."
        fun.version <- "threeDarr 004"

	dots <- list(...)
	ndot <- length(dots)
	if(ndot == 1 && rep <= 1) {
		stop(paste("expecting at least 2 inputs when 'rep'",
			"is not greater than 1"))
	} else if(ndot == 0) {
		stop("no inputs given")
	}
	ddims <- lapply(dots, dim)
	if(any(unlist(lapply(ddims, length)) != 2)) {
		stop(paste(sum(unlist(lapply(ddims, length)) != 2),
			"input(s) do not have length 2 dim"))
	}
	ddimnam <- lapply(dots, dimnames)
	if(ndot == 1) {
		rnam <- ddimnam[[1]][[1]]
		cnam <- ddimnam[[1]][[2]]
		rnc <- nchar(rnam)
		cnc <- nchar(cnam)
		if(any(rnc == 0)) {
			rnsub <- paste("R", 1:length(rnam), sep="")
			rnam[rnc == 0] <- rnsub[rnc == 0]
			dimnames(dots[[1]])[[1]] <- rnam
		}
		if(any(cnc == 0)) {
			cnsub <- paste("C", 1:length(cnam), sep="")
			cnam[cnc == 0] <- cnsub[cnc == 0]
			dimnames(dots[[1]])[[2]] <- cnam
		}
	} else {
		rtest <- unlist(lapply(ddimnam, function(x) length(x[[1]])))
		ctest <- unlist(lapply(ddimnam, function(x) length(x[[2]])))
		if((any(rtest == 0) && any(rtest > 0)) || (any(ctest == 0) &&
				any(ctest > 0))) {
			stop(paste(sum(rtest == 0), "inputs without rownames",
				"and", sum(ctest == 0), "without colnames",
				"while at least one does have these"))
		}
		if(union) {
			rnam <- unique(unlist(lapply(ddimnam, function(x) 
				x[[1]])))
			cnam <- unique(unlist(lapply(ddimnam, function(x) 
				x[[2]])))
		} else {
			rnam <- ddimnam[[1]][[1]]
			cnam <- ddimnam[[1]][[2]]
			if(ndot > 1) {
				for(i in 2:ndot) {
					rnam <- intersect(rnam, 
						ddimnam[[i]][[1]])
					cnam <- intersect(cnam, 
						ddimnam[[i]][[2]])
				}
			}
		}
	}
	if(any(nchar(rnam) == 0) || any(duplicated(rnam))) {
		stop("when row names exist, all rows must have unique names")
	}
	if(any(nchar(cnam) == 0) || any(duplicated(cnam))) {
		stop(paste("when column names exist, all columns must have",
			"unique names"))
	}
	if(!length(rnam)) {
		nr <- unlist(lapply(ddims, function(x) x[1]))
		if(diff(range(nr))) {
			stop(paste("no (suitable) row names and variable",
				"number of rows in inputs"))
		}
		nr <- nr[1]
	} else {
		nr <- length(rnam)
	}
	if(!length(cnam)) {
		nc <- unlist(lapply(ddims, function(x) x[2]))
		if(diff(range(nc))) {
			stop(paste("no (suitable) column names and variable",
				"number of columns in inputs"))
		}
		nc <- nc[1]
	} else {
		nc <- length(cnam)
	}
	if(length(slicenames)) {
		if(length(slicenames) != ndot * rep) {
			stop(paste("length of 'slicenames' is", 

	ans
}

