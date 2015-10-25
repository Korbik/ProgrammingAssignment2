## These functions enable the caching of the invert of a matrix by using getters and setters by function 
## on a matrix capsule (caching the computed invert matrix)

## This function create a cacheable matrix with the default empty matrix
## and return a list of function to modify the matrix or the cached invert of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function solves the invert of the 'x' cacheable matrix. 'x' must have been created 
## with makeCacheMatrix() and set using x$set and returns a matrix that is the inverse of 'x'
## the '...' allow to pass others arguments to the solve method

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
