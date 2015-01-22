## The makeCacheMatrix function calculates the inverse of x, a vector input.
## The cacheSolve matrix returns this cached value if the input is
## the same as what was given to the makeCacheMatrix function. Note:
## the makeCacheMatrix function is best used if its output is assigned
## to variables (e.g. "a", "b", "c"), each one receiving different values
## for x.

## This function takes a vector, x, and coerces it as a matrix.
## It then takes the inverse of x and stores it as a list.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m
	list(set = set, get = get,
		   setmatrix = setmatrix,
		   getmatrix = getmatrix)
}

## This function retrieves the inverse of x, as performed by
## the makeCacheMatrix function. The output gives the inverse
## and stores it as a matrix.

cacheSolve <- function(x = matrix(), ...) {
	m <- x$getmatrix()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setmatrix(m)
	return(m)
}
