## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
## Basically the same as the makevector

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) s <<- solve
	getInverse <- function() s
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)

}


## Write a short comment describing this function
## Return a matrix that is the inverse of matrix returned by 
## "makeCacheMatrix"

cacheSolve <- function(x, ...) {        
		s <- x$getInverse()
		if(!is.null(s)) {
			message("getting cached data")
			return(s)
		}
		data <- x$get()
		s <- solve(data, ...)
		x$setInverse(s)
		s
}
