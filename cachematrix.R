## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly.  write a pair of functions that
## cache the inverse of a matrix.

## By definition, the inverse is written A raised to the -1 power. When A is multiplied by 
## A to the -1 power the result is the identity matrix I. Non-square matrices do not have inverses.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## it will (similar to the example makeVector):

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## 1. set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## 2. get the value of the matrix
    get <- function() x
    
    ## 3. set the value of the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    ## 4. get the value of the inverse
    getinverse <- function() inv
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Get the Inverse of x
    invM <- x$getinverse()
    
    ## Determine the the invM (Inverse Matrix) variable is not null
    if (!is.null(invM)) {
       ## If it is not null then dispay the message "getting cached data
       ## and return the inverse Matrix
        message("getting cached data")
        return(invM)
    }
    
    ## Get the Matrix
    data <- x$get()
    ## Inverse the matrix using the solve function
    invM <- solve(data, ...)
    ## set in Inverse Matrix in Memory
    x$setinverse(invM)
    ## Return the Inverse Matrix
    invM
}
