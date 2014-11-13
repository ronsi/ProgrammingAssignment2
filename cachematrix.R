#---------------------------------------------------
# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly (there are also alternatives to matrix inversion that we will
# not discuss here). Your assignment is to write a pair of functions that
# cache the inverse of a matrix.
#---------------------------------------------------

# `makeCacheMatrix`: This function creates a special "matrix" object
#    that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	# define a set function to set the cached value
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	# get the raw matrix
	get <- function() x
	
	# set the cached value
	setsolve <- function(solve) s <<- solve
	
	# return the cached value
	getsolve <- function() s
	
	# define interface
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}


# main function, returns the inverse of a matrix
#`cacheSolve`: This function computes the inverse of the special
#    "matrix" returned by `makeCacheMatrix` above. If the inverse has
#    already been calculated (and the matrix has not changed), then
#    `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	
	# get the inverse from the cache
	s <- x$getsolve()
	
	# is there a cached value?
        if(!is.null(s)) {
		message("getting cached data")
                return(s)
	}
        
        # get the raw matrix
        data <- x$get()
        
        # calculate the inverse
        s <- solve(data, ...)
        
        # set the result to the cache
        x$setsolve(s)
        
        # return value
        s
}
