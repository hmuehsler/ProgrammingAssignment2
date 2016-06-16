## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function caches the inverse of a matrix.
## 
## Here are two functions that are made to store a matrix and cache the inverse.


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The next function finds the inverse of the matrix found above. 
## If the inverse was already found and the matrix is the same, the the cached
## matrix is used.

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}