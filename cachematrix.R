#### A pair of functions for making matrices that can solve and internally cache their own values


## Create a matrix with built-in caching for inversions

makeCacheMatrix <- function(x = matrix()) {
	# Inner value containing the inverse matrix
	m <- NULL
	
	# Changing the data (x) invalidates any existing inversion (m)
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() {
		x
	}
	
	# Inversion-related functions. These are only called by cacheSolve()
	setInv <- function(y) {
		m <<- y
	}
	getInv <- function() {
		m
	}
	
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Return inverse of makeCacheMatrix from cache, performing calculation and storing result if missing.

cacheSolve <- function(x, ...) {
	# Retrieve cached value from makeCacheMatrix(), or NULL if no value present.
	m = x$getInv()
	
	if(is.null(m)) {
		message("Performing inversion and updating cache")
		m <- x$setInv(solve(x$get()))
	}
	else {
		message("Returning cached inversion")
		
	}
	
	m
}
