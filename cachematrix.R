## This group of funtions will cache the inverse of a matrix and cache that 
## result. When retrying to calculate the inverse of a matrix the inverse is 
## first checked for availability in the cache.

## Create a vector to be used by the second function 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function() inv <<- solve(x) 
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Check if the matrix as already been inversed and is stored in the cache. If 
## not, calculate the inverse

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("Retrieving cached inverted matrix")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
