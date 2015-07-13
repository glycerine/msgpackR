unpack <-
function(str) {
	.unpack_bin <- function(bin) {
		e$.msgpack_index <- 1
		e$.msgpack_data <- bin
	
		return(.unpack_data())
	}

	.unpack_file <- function(filename) {
		fl <- file(filename, "rb")
		bits <- readBin(fl, raw(), file.info(filename)$size)
		close(fl)
	
		e$.msgpack_index <- 1
		e$.msgpack_data <- bits
		return(.unpack_data())
	}

	
	.unpack_pfixnum <- function() {
		num <- e$.msgpack_data[e$.msgpack_index]
		e$.msgpack_index <- e$.msgpack_index + 1
		return(as.integer(num & as.raw(0x7F)))
	}

	.unpack_fixmap <- function() {
		result <- list()
		N <- as.integer(e$.msgpack_data[e$.msgpack_index] & as.raw(0x0F))
		nms <- character(N)
		
		e$.msgpack_index <- e$.msgpack_index + 1
		
		for ( i in 1:N ) {
			nms[i] <- .unpack_data()
			result[[i]] <- .unpack_data()
		}
		result <- .unpack_checkclass(result)
		names(result) <- nms
		return(result)
	}

	.unpack_fixarray <- function() {
		result <- list()
		N <- as.integer(e$.msgpack_data[e$.msgpack_index] & as.raw(0x0F))
		
		e$.msgpack_index <- e$.msgpack_index + 1
		
		for ( i in 1:N ) {
			result[[i]] <- .unpack_data()
		}
		result <- .unpack_checkclass(result)
		
		return(result)
	}

	.unpack_fixstr <- function() {
		result <- ""
		result_byte <- list()
		N <- as.integer(e$.msgpack_data[e$.msgpack_index] & as.raw(0x1F))
		
		e$.msgpack_index <- e$.msgpack_index + 1
		
		for ( i in 1:N ) {
			result_byte[[i]] <- rawToChar(e$.msgpack_data[e$.msgpack_index])
			e$.msgpack_index <- e$.msgpack_index + 1
		}

		result_byte <- .unpack_checkclass(result_byte)
		
		for ( i in 1:length(result_byte) ) {
			result <- paste(result, result_byte[i], sep="")
		}

		return(result)
	}

	.unpack_nil <- function() {
		e$.msgpack_index <- e$.msgpack_index + 1
		return(NULL)
	}

	.unpack_false <- function() {
		e$.msgpack_index <- e$.msgpack_index + 1
		return(FALSE)
	}

	.unpack_true <- function() {
		e$.msgpack_index <- e$.msgpack_index + 1
		return(TRUE)
	}

	.unpack_float32 <- function() {
		# R supports not "float" but "double".
		return(.unpack_float64())
	}

	.unpack_float64 <- function() {
		result <- 0
		e$.msgpack_index <- e$.msgpack_index + 1
	
		bits <- c()
		for ( i in 1:8 ) {
			bits[i] <- e$.msgpack_data[e$.msgpack_index]
			e$.msgpack_index <- e$.msgpack_index + 1
		}
	
		# sign
		sign <- ifelse((bits[1] & as.raw(0x80)) != as.raw(0x00), -1, 1)
		# exponent
		exp <- as.integer(bits[1] & as.raw(0x7F))*2^4 + as.integer(bits[2] & as.raw(0xF0))/2^4 - 1023
		# fraction
		frac <- (rev(c(rawToBits(bits[2] & as.raw(0x0F)))))[5:8]
		for ( i in 3:8 ) {
			frac <- c(frac, as.integer(rev(rawToBits(bits[i]))))
		}
	
		for ( i in 1:52 ) {
			result <- result + frac[i]/2^i
		}
		result <- sign*(result+1)*2^exp
		
		return(result)
	}

	.unpack_uint8 <- function() {
		result <- 0
		
		e$.msgpack_index <- e$.msgpack_index + 1
		result <- result + as.integer(e$.msgpack_data[e$.msgpack_index])
		
		e$.msgpack_index <- e$.msgpack_index + 1
		return(result)
	}

	.unpack_uint16 <- function() {
		result <- 0
		
		for ( i in seq(8,0,-8) ) {
			e$.msgpack_index <- e$.msgpack_index + 1
			result <- result + as.integer(e$.msgpack_data[e$.msgpack_index])*2^i
		}
		
		e$.msgpack_index <- e$.msgpack_index + 1
		return(result)
	}

	.unpack_uint32 <- function() {
		result <- 0
		
		for ( i in seq(24,0,-8) ) {
			e$.msgpack_index <- e$.msgpack_index + 1
			result <- result + as.integer(e$.msgpack_data[e$.msgpack_index])*2^i
		}
		
		e$.msgpack_index <- e$.msgpack_index + 1
		return(result)
	}

	.unpack_uint64 <- function() {
		result <- 0
		
		for ( i in seq(56,0,-8) ) {
			e$.msgpack_index <- e$.msgpack_index + 1
			result <- result + as.integer(e$.msgpack_data[e$.msgpack_index])*2^i
		}
		
		e$.msgpack_index <- e$.msgpack_index + 1
		return(result)
	}

	.unpack_int8 <- function() {
		result <- 0
		
		e$.msgpack_index <- e$.msgpack_index + 1
		sign <- ifelse((e$.msgpack_data[e$.msgpack_index] & as.raw(0x80)) != as.raw(0x00), -1, 1)
		
		result <- result + as.integer(e$.msgpack_data[e$.msgpack_index])
		e$.msgpack_index <- e$.msgpack_index + 1
		
		if ( sign < 0 ) {
			result <- -2^8 + result
		}
		
		return(result)
	}

	.unpack_int16 <- function() {
		result <- 0
		
		e$.msgpack_index <- e$.msgpack_index + 1
		sign <- ifelse((e$.msgpack_data[e$.msgpack_index] & as.raw(0x80)) != as.raw(0x00), -1, 1)
		
		for ( i in seq(8,0,-8) ) {
			result <- result + as.integer(e$.msgpack_data[e$.msgpack_index])*2^i
			e$.msgpack_index <- e$.msgpack_index + 1
		}
		
		if ( sign < 0 ) {
			result <- -2^16 + result
		}
		
		return(result)
	}

	.unpack_int32 <- function() {
		result <- 0
		
		e$.msgpack_index <- e$.msgpack_index + 1
		sign <- ifelse((e$.msgpack_data[e$.msgpack_index] & as.raw(0x80)) != as.raw(0x00), -1, 1)
		
		for ( i in seq(24,0,-8) ) {
			result <- result + as.integer(e$.msgpack_data[e$.msgpack_index])*2^i
			e$.msgpack_index <- e$.msgpack_index + 1
		}
		
		if ( sign < 0 ) {
			result <- -2^32 + result
		}
		
		return(result)
	}

	.unpack_int64 <- function() {
		result <- 0
		
		e$.msgpack_index <- e$.msgpack_index + 1
		sign <- ifelse((e$.msgpack_data[e$.msgpack_index] & as.raw(0x80)) != as.raw(0x00), -1, 1)
		
		for ( i in seq(56,0,-8) ) {
			result <- result + as.integer(e$.msgpack_data[e$.msgpack_index])*2^i
			e$.msgpack_index <- e$.msgpack_index + 1
		}
		
		if ( sign < 0 ) {
			result <- -2^64 + result
		}
		
		return(result)
	}


	.unpack_bin8 <- function() {
		result <- list()
		
		e$.msgpack_index <- e$.msgpack_index + 1		
        N <- as.integer(e$.msgpack_data[e$.msgpack_index])

        e$.msgpack_index <- e$.msgpack_index + 1
		
		for ( i in 1:N ) {
			result[[i]] <- e$.msgpack_data[e$.msgpack_index]
			e$.msgpack_index <- e$.msgpack_index + 1
		}
		result <- unlist(result)
		
		return(result)
	}

	.unpack_bin16 <- function() {
		result <- list()
		N <- 0
		
		e$.msgpack_index <- e$.msgpack_index + 1
		
		for ( i in seq(8,0,-8) ) {
			N <- N + as.integer(e$.msgpack_data[e$.msgpack_index])*2^i
			e$.msgpack_index <- e$.msgpack_index + 1
		}
		
		for ( i in 1:N ) {
			result[[i]] <- e$.msgpack_data[e$.msgpack_index]
			e$.msgpack_index <- e$.msgpack_index + 1
		}
		result <- unlist(result)
		
		return(result)
	}    
    
	.unpack_bin32 <- function() {
		result <- list()
		N <- 0
		
		e$.msgpack_index <- e$.msgpack_index + 1
		
		for ( i in seq(24,0,-8) ) {
			N <- N + as.integer(e$.msgpack_data[e$.msgpack_index])*2^i
			e$.msgpack_index <- e$.msgpack_index + 1
		}
		
		for ( i in 1:N ) {
			result[[i]] <- e$.msgpack_data[e$.msgpack_index]
			e$.msgpack_index <- e$.msgpack_index + 1
		}
		result <- unlist(result)
		
		return(result)
	}
    
	.unpack_str8 <- function() {
		result <- list()
		
		e$.msgpack_index <- e$.msgpack_index + 1		
        N <- as.integer(e$.msgpack_data[e$.msgpack_index])

        e$.msgpack_index <- e$.msgpack_index + 1
		
		for ( i in 1:N ) {
			result[[i]] <- rawToChar(e$.msgpack_data[e$.msgpack_index])
			e$.msgpack_index <- e$.msgpack_index + 1
		}
		result <- .unpack_checkclass(result)
		
		return(result)
	}

	.unpack_str16 <- function() {
		result <- list()
		N <- 0
		
		e$.msgpack_index <- e$.msgpack_index + 1
		
		for ( i in seq(8,0,-8) ) {
			N <- N + as.integer(e$.msgpack_data[e$.msgpack_index])*2^i
			e$.msgpack_index <- e$.msgpack_index + 1
		}
		
		for ( i in 1:N ) {
			result[[i]] <- rawToChar(e$.msgpack_data[e$.msgpack_index])
			e$.msgpack_index <- e$.msgpack_index + 1
		}
		result <- .unpack_checkclass(result)
		
		return(result)
	}

	.unpack_str32 <- function() {
		result <- list()
		N <- 0
		
		e$.msgpack_index <- e$.msgpack_index + 1
		
		for ( i in seq(24,0,-8) ) {
			N <- N + as.integer(e$.msgpack_data[e$.msgpack_index])*2^i
			e$.msgpack_index <- e$.msgpack_index + 1
		}
		
		for ( i in 1:N ) {
			result[[i]] <- rawToChar(e$.msgpack_data[e$.msgpack_index])
			e$.msgpack_index <- e$.msgpack_index + 1
		}
		result <- .unpack_checkclass(result)
		
		return(result)
	}

	.unpack_array16 <- function() {
		result <- list()
		N <- 0
		
		e$.msgpack_index <- e$.msgpack_index + 1
		
		for ( i in seq(8,0,-8) ) {
			N <- N + as.integer(e$.msgpack_data[e$.msgpack_index])*2^i
			e$.msgpack_index <- e$.msgpack_index + 1
		}
		
		for ( i in 1:N ) {
			result[[i]] <- .unpack_data()
		}
		result <- .unpack_checkclass(result)
		
		return(result)
	}

	.unpack_array32 <- function() {
		result <- list()
		N <- 0
		
		e$.msgpack_index <- e$.msgpack_index + 1
		
		for ( i in seq(24,0,-8) ) {
			N <- N + as.integer(e$.msgpack_data[e$.msgpack_index])*2^i
			e$.msgpack_index <- e$.msgpack_index + 1
		}
		
		for ( i in 1:N ) {
			result[[i]] <- .unpack_data()
		}	
		result <- .unpack_checkclass(result)
		
		return(result)
	}

	.unpack_map16 <- function() {
		result <- list()
		N <- 0
		
		e$.msgpack_index <- e$.msgpack_index + 1
		
		for ( i in seq(8,0,-8) ) {
			N <- N + as.integer(e$.msgpack_data[e$.msgpack_index])*2^i
			e$.msgpack_index <- e$.msgpack_index + 1
		}
		
		nms <- character(N)
		
		for ( i in 1:N ) {
			nms[i] <- .unpack_data()
			result[[i]] <- .unpack_data()
		}
		result <- .unpack_checkclass(result)
		names(result) <- nms
		
		return(result)
	}

	.unpack_map32 <- function() {
		result <- list()
		N <- 0
		
		e$.msgpack_index <- e$.msgpack_index + 1
		
		for ( i in seq(24,0,-8) ) {
			N <- N + as.integer(e$.msgpack_data[e$.msgpack_index])*2^i
			e$.msgpack_index <- e$.msgpack_index + 1
		}
		
		nms <- character(N)
			
		for ( i in 1:N ) {
			nms[i] <- .unpack_data()
			result[[i]] <- .unpack_data()
		}
		result <- .unpack_checkclass(result)
		names(result) <- nms
		
		return(result)
	}

	.unpack_nfixint <- function() {
		num <- e$.msgpack_data[e$.msgpack_index]
		e$.msgpack_index <- e$.msgpack_index + 1
		return(-32 + as.integer(num & as.raw(0x1F)))
	}

	.unpack_checkclass <- function(data) {
		classes <- unique(unlist(lapply(data, class)))

		for ( i in 1:length(classes) ) {
			if ( classes[i] == "integer" ) {
				classes[i] = "numeric"
			}
		}
		classes <- unique(classes)
		
		len <- unique(unlist(lapply(data, length)))
		if( length(classes) == 1 && length(len) == 1 && len[1] == 1) {
			class(data) <- classes
		}
		
		return(data)
	}

    ## todo
	.unpack_ext8 <- function() { stop("todo: implement .unpack_ext8") }
	.unpack_ext16 <- function() { stop("todo: implement .unpack_ext16") }
	.unpack_ext32 <- function() { stop("todo: implement .unpack_ext32") }
	.unpack_fixext1 <- function() { stop("todo: implement .unpack_fixext1") }
	.unpack_fixext2 <- function() { stop("todo: implement .unpack_fixext2") }
	.unpack_fixext4 <- function() { stop("todo: implement .unpack_fixext4") }
	.unpack_fixext8 <- function() { stop("todo: implement .unpack_fixext8") }
	.unpack_fixext16 <- function() { stop("todo: implement .unpack_fixext16") }

    ## end todo
    
	.unpack_data <- function() {
		if ( is.null(e$.msgpack_data) ) {
			return(NULL)
		}
		# Positive FixInt
		else if( e$.msgpack_data[e$.msgpack_index] <= as.raw(0x7F) ) {
			return(.unpack_pfixnum())
		}
		# FixMap
		else if ( e$.msgpack_data[e$.msgpack_index] <= as.raw(0x8F) ) {
			return(.unpack_fixmap())
		}
		# FixArray
		else if ( e$.msgpack_data[e$.msgpack_index] <= as.raw(0x9F) ) {
			return(.unpack_fixarray())
		}
		# FixStr
		else if ( e$.msgpack_data[e$.msgpack_index] <= as.raw(0xBF) ) {
			return(.unpack_fixstr())
		}
		# nil
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xC0) ) {
			return(.unpack_nil())
		}
		# (reserved)
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xC1) ) {
			return(NULL)
		}
		# false
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xC2) ) {
			return(.unpack_false())
		}
		# true
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xC3) ) {
			return(.unpack_true())
          }
		# bin8
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xC4) ) {
			return(.unpack_bin8())
          }
		# bin16
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xC5) ) {
			return(.unpack_bin16())
          }
		# bin32
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xC6) ) {
			return(.unpack_bin32())
          }
		# ext8
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xC7) ) {
			return(.unpack_ext8())
          }
		# ext16
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xC8) ) {
			return(.unpack_ext16())
          }
		# ext32
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xC9) ) {
			return(.unpack_ext32())
          }
		# float32
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xCA) ) {
			return(.unpack_float32())
		}
		# float64
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xCB) ) {
			return(.unpack_float64())
		}
		# uint8
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xCC) ) {
			return(.unpack_uint8())
		}
		# uint16
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xCD) ) {
			return(.unpack_uint16())
		}
		# uint32
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xCE) ) {
			return(.unpack_uint32())
		}
		# uint64
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xCF) ) {
			return(.unpack_uint64())
		}
		# int8
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xD0) ) {
			return(.unpack_int8())
		}
		# int16
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xD1) ) {
			return(.unpack_int16())
		}
		# int32
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xD2) ) {
			return(.unpack_int32())
		}
		# int64
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xD3) ) {
			return(.unpack_int64())
          }
   		# fixext1
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xD4) ) {
			return(.unpack_fixext1())
          }
   		# fixext2
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xD5) ) {
			return(.unpack_fixext2())
          }
   		# fixext4
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xD6) ) {
			return(.unpack_fixext4())
          }
   		# fixext8
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xD7) ) {
			return(.unpack_fixext8())
          }
   		# fixext16
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xD8) ) {
			return(.unpack_fixext16())
          }
		# str8
		else if ( e$.msgpack_data[e$.msgpack_index] <= as.raw(0xD9) ) {
			return(.unpack_str8())
          }
		# str16
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xDA) ) {
			return(.unpack_str16())
		}
		# str32
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xDB) ) {
			return(.unpack_str32())
		}
		# array16
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xDC) ) {
			return(.unpack_array16())
		}
		# array32
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xDD) ) {
			return(.unpack_array32())
		}
		# map16
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xDE) ) {
			return(.unpack_map16())
		}
		# map32
		else if ( e$.msgpack_data[e$.msgpack_index] == as.raw(0xDF) ) {
			return(.unpack_map32())
		}
		# Negative FixInt
		else {
			return(.unpack_nfixint())
		}
	}

	e <- new.env()
	e$.msgpack_index <- 1
	e$.msgpack_data <- list()
	# the case when str is filename
	if ( mode(str) == "character" ) {
		return(.unpack_file(str))
	}
	# the case when str is raw array
	else if ( mode(str) == "raw" ) {
		return(.unpack_bin(str))
	}
	else {

	}
}
