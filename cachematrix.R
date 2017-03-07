## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix function creates a matrix
## that we can use to cache the inverse of the matrix itself
## It will set the value of the matrix
## get the value of the matrix
## then set the value of the inverse
## and finally get the value inverse

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    set <- function(y) {
        x <<- y
        mi <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) mi <<- inverse
    getinverse <- function() mi
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

## This function calculates the inverse of the matrix created
## in makeCacheMatrix and it caches the answer if it has already
## been calculated using it before.  
## in this case it will get the inverse from the cache and not
## re-calculate it.  Otherwise, it will calcaluate the inverse
## and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mi <- x$getinverse()
    if(!is.null(mi)) {
        message("getting cached data")
        return(mi)
    }
    data <- x$get()
    mi <- solve(data, ...)
    x$setinverse(mi)
    mi
}



