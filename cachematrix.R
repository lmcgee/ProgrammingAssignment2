## Put comments here that give an overall description of what your
## functions do

## This function takes the input matrix and creates the cache inverse to be used later

makeCacheMatrix <- function(x = matrix()) {
    ## print("set function called")  - used for testing
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ## print("get function called")  - used for testing
    
    get <- function() x
    setinverse <- function(inverse) inverse <<- inverse
    getinverse <- function() inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
}

## Write a short comment describing this function

## This function creates the inverse for cache retrieval. 
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("into the cached data.")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}