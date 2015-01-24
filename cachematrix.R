## The functions below create a special matrix object that can 
## compute and cache an inverse, checking first if it can skip 
## the computation to speed up the process. 

## Assume that the matrix supplied is always invertible

## The "makeCacheMatrix" function creates a "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}

## The "cacheSolve" function computed the inverse of the special 
## "matrix" returned by the "makeCacheMatrix" function above. 
## If the inverse has already been calculated, and the matrix 
## has not been changed, then 'cacheSolve' retrieves the inverse 
## from the cache. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
