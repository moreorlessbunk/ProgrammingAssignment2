## This R script contains a set of functions that will cache a matrix and its
##inverse, and return a matrix and its inverse.

## Function 1: makeCacheMatrix produces a list of functions to cache and
## retrieve the inverse matrix

## Function 2: cachSolve returns the inverse of a matrix



## List of functions: set and return the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        # returns the value of x
        get <- function () x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list <-(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## checks cache for the inverse of a matrix x and returns,
## if no inverse is stored in chache, computes and returns inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
