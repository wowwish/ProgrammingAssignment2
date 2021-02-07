## These two function show the usage of caching in R to store the 
## inverse of a matrix in an environment above the current environment

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL 
	}
	get <- function() y
	setinv <- functon(i) inv <<- i
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(x)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(x, ...)
	x$setinv(inv)
	inv
}
