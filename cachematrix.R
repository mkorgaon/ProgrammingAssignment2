## Matrix inversion is usually a costly computation and there are benefit to caching 
## the inverse of a matrix rather than compute it repeatedly.
## makeCacheMatrix and cacheSolve are a pair of functions that compute and 
## cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {            ## set the value of the matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x             ## get the value of the matrix
        setInverse <- function(inverse) inv <<- inverse ## set the value of the inverse
        getInverse <- function() inv    ## get the value of the inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated and the matrix has not changed, then the 
## cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()                     # query the x matrix's cache
        if(!is.null(m)) {                       # if there is a cache
                message("getting cached data")
                return(m)                       # just return the cache, no computation needed
        }
        data <- x$get()                         # if there's no cache
        m <- solve(data, ...)                   # compute matrix inverse
        x$setInverse(m)                         # save the result back to x's cache
        m                                       # return a matrix that is the inverse of 'x'
}
