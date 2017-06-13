## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Return a list of functions that describe a matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # reset the inverse matrix
    set <- function(y) {
        x <<- y     # set the matrix
        i <<- NULL  # reset the inverse of the matrix
    }
    get <- function() x     # get the matrix
    setinverse <- function(inv) i <<- inv       # set the inverse matrix
    getinverse <- function() i                  # get the inverse of the matrix

    # return the list of the get/set functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    i <- x$getinverse()     # get the cached inverse of the matrix
    if(!is.null(i)) {  # if the cached inverse is not null, return it directly
        message("getting cached data")
        return(i)      # return the cached inverse since it is not NULL
    }
    # continue if the inverse is NULL
    data <- x$get()  # get the matrix x
    i <- solve(data, ...)   # calculate the inverse of x
    x$setinverse(i)  # cache the inverse
    i                # return the inverse
}
