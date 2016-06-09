## The two following functions are used to cache and retrieve the inverse of a matrix.
## Because matrix inversion is usually a costly computation, caching the inverse of a
## matrix may provide some benefits as apposed to computing it repeatedly.

## The following function creates a special "matrix" object that can cache its inverse.
## The function:
## 1. sets the value of the matrix 
## 2. gets the value of the matrix 
## 3. sets the value of inverse of the matrix 
## 4. gets the value of inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) invrs <<- solve
        getinverse <- function() invrs
        list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve function retrieves the inverse from
## the cache.

## This function assumes that the matrix supplied is always invertible. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getinverse()
        if(!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
}
        data <- x$get()
        invrs <- solve(data, ...)
        x$setinverse(invrs)
        invrs
}