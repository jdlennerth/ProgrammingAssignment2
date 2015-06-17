## Created Date:    6/16/2015
## Created by:      Jeff Lennerth
## Purpose:

## Creates an object that can cache matrixes 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list (set = set, get= get,
            setinverse = setinverse,
            getinverse = getinverse)
    
}
  


## calls solve on given Matrix (via makeCacheMatrix) or pulls the 
## cached version of the previously calculated inverse of the matrix.
## run in conjunction with makeCacheMatrix like so:
## x <- matrix(c(3,4,1,2),nrow = 2, ncol = 2)
## f <- makeCacheMatrix(x)
## cacheSolve(f)  will give the proper inverse [1, -.5 over -2, 1.5]

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
