## makeCacheMatrix: This function creates a special "matrix" object 
##that can cache its inverse.
##cacheSolve: This function computes the 
##inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache

## Given a matrix this function assigns getter and setter functions for the
## matrix value and it's inverse. If a new matrix value is set, it's
## inverse is set to NULL
## Note values of both setting matrix value & it's inverse work by reference

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setinv <- function(inv) invmat <<- inv
        getinv <- function() invmat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Checks if the cache contains the inverted value
## if it does it returns the value, skips the computation
## if it doesn't it computes it using the solve function 
## and sets it in the makeCacheMatrix function by reference
## This needs to be done only once

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
