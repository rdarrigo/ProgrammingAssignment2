## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setsolve <- function(s) inverse <<- s
    getsolve <- function() inverse
    list( set = set 
        , get  = get
        , setsolve  = setsolve 
        , getsolve  = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.
## Assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getsolve()
    if(!is.null(inverse)) {
        message('getting cached data')
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setsolve(inverse)
    inverse
}
