## Set of functions to store a matrix and its inverted couterpart

## Stores the matrix and its inverted counterpart. The inverted matrix is cached
## to allow fast access

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    set <- function(y) {
        x <<- y
        inverted <<- NULL
    }
    get <- function() x
    setInverted <- function(inv) inverted <<- inv
    getInverted <- function() inverted
    list(set=set, get=get,
         setInverted = setInverted,
         getInverted = getInverted)
}


## This function stores a copy of the inverted matrix if matrix has not changed.
## Saves a computationnaly intensive step when multiple access to the inverted
## matrix might prove necessary

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverted <- x$getInverted()
    if (!is.null(inverted)) {
        message("Getting cached copy of inverse")
        return(inverted)
    }
    mat <- x$get()
    inverted <- solve(mat, ...)
    x$setInverted(inverted)
    inverted
}
