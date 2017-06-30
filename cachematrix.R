## This module calculates and caches the inverse of a matrix functions do

## This function builds the cache functionality by using a lexical closure.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
        setmatrix <- function(mat) m <<- mat
        getmatrix <- function() m
        list(set = set, get = get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## This function checks to see if we have an inverse for this matrix
## If we do, then we return it
## If we don't, then we calculate and cache it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
