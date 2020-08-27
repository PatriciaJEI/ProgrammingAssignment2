# This functions calculate and store in memory the inverse of an 
# invertible matrix to avoid repeating calculations.

## This first function creates a list that aims to store a matrix
## and its inverse by means of four functions. "set" allows to
## store a new different matrix in the function environment and 
## resets its inverse. "setinverse" stores the new value of the 
## inverse in the function environment (that will be given by the
## second function: cacheSolve). "get" and "getinverse" get the 
## objects provided and "cached" by "set" and "setinverse".

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <-function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This second function evaluates the cache list created by the
## first function. If it is the first time, this function calculates
## the inverse of the matrix provided by the cache list and stores
## the result in the named list. If it isn't the first time, the 
## function looks up the inverse of the matrix in the environment
## of the cacheMatrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("Getting cache data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}
