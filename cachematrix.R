## These functions implement a special matrix-like object that cache its inverse.
## The code is a slightly edited version of the example code for makeVector and cachemean
## given in the instructions to the assignment.
##
## Usage:
## cx<-makecacheMatrix(x)
## invx<-cacheSolve(cx,...)
##
## Arguments:
## x - a square matrix; An error may occur when x is not invertible.
## cx - the cached matrix object.
## ... - additional arguments to cacheSolve are sent to solve().
##
## Details:
## Use cx$get() to access the original matrix.
## After the first call to cacheSolve, the cached value of the inverse is returned.
## The additional arguments to cacheSolve are used only on when actually computing
## the inverse, not when returning cached values.


## makeCacheMatrix creates the cached matrix object. The data of the object consists of
## x - the matrix
## invx - the inverse of x
##
## cm<-makeCacheMatrix(x) returns cm, a list of four functions:
## cm$set() sets the matrix and delete the cached inverse value
## cm$get() returns the matrix
## cm$setinv() sets the inverse of the matrix; note that no checks are actually done 
##     to ensure that this is the true inverse, and therefore this functions should not
##     not be used (it is only called by cacheSolve)
## cm$getinv() gets the inverse of the matrix; like setinv, it should not be used
## directly, only through cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL 
    ## set: sets the value of the cached matrix (x) and nulls the cached inverse (invx)
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
    ## get: just returns the cached matrix
        get <- function() x
    ## setinv: sets the value of the inverse 
        setinv <- function(inv) invx <<- inv
    ## getinv: gets the value of the inverse
        getinv <- function() invx
    ## return value: a list composed of the four functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## invx<-cacheSolve(cm) returns the inverse of the cached matrix cm, previously
## created by makecacheMatrix.
## cacheSolve(cm) uses the R function solve() to actually compute the inverse, but
## only if it has not been computed before. It uses the cm$getinv() and cm$setinv()
## functions for that purpose.

cacheSolve <- function(cm, ...) {
        invx <- cm$getinv()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        data <- cm$get()
        invx <- solve(data,...)
        cm$setinv(invx)
        invx
 
}
