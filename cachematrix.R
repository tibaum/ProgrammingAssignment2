## The functions makeCacheMatrix and cacheSolve work together to compute and
## cache the inverse of a matrix. Function calls for repeated computations
## return the cached inverse instead of computing it again.


## This function creates an object to store a matrix and cache its inverse once
## that it is computated by the function cacheSolve.
## The object created by makeCacheMatrix provides the following functions:
##      get(): returns the matrix itself
##      set(new_matrix): replaces the matrix and deletes the cached inverse
##      get_inverse(): returnes the cached inverse if present, else NULL
##      set_inverse(inverse_matrix): puts the inverse in the cache
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(new_matrix) {
        x <<- new_matrix
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    set_inverse <- function(inverse_matrix) {
        inverse <<- inverse_matrix
    }
    get_inverse <- function() {
        inverse
    }
    list(set = set,
         get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## This function computes the inverse of a matrix that was created by the
## function makeCacheMatrix and stores it in the cache. If the cache already
## contains the inverse then it is retrieved from there.
cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    if (!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    inverse <- solve(x$get())
    x$set_inverse(inverse)
    inverse
}
