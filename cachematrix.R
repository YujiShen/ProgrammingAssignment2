##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repetedely. This file contains two functions to cache the inverse of a matrix.

##This function computes the inverse of the special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setSolve <- function(inverse) im <<- inverse
        getSolve <- function() im
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        im <- x$getSolve()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setSolve(im)
        im
}

