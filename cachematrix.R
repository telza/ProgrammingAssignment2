## These functions take into advantage R's scoping rules to cache 
## the results of an inverse of a matrix if it was computed before.
## This is done to avoid repeating calculation of an inverse of a maptrix.

## Assumptiont: the matrices sent as parameters are always inversible

## The makeCacheMatrix function creates a special "matrix",
## which is really a list containing a function to :
##  1. get the value of the matrix
##  2. set the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL 
    set <- function (y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) {
        i <<- inverse
    }
    getinverse <- function() i 
    list(set=set, get=get, 
         setinverse=setinverse, getinverse=getinverse )
}


## The following function calculates the inverse of the special "matrix" 
# created with makeCacheMatrix. However, it first checks to see if the inverse
# has already been calculated. If so, it gets the inverse from the cache and 
# skips the computation. Otherwise, it calculates the inverse of the data 
# and sets the value of the mean in the cache via the setmean function.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)){
        message("getting cached data")
        i
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
    
