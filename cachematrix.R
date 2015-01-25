## The code in this file allows creation and manipulation of matrices that
## can cache their inverses.

## makeCacheMatrix takes a matrix as a parameter, and returns an object that
## contains the matrix and is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # cached inverse; initially NULL
    set <- function(y) {
        # set new matrix content and invalidate the now outdated cache
        x <<- y
        inv <<- NULL
    }
    get <- function() x # get the matrix
    setInverse <- function(inverse) inv <<- inverse # set the cached inverse
    getInverse <- function() inv # get the cached inverse
    # return the object that contains the above functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolveWrite checks if the "cacheMatrix" object given as parameter contains
## a valid cached inverse, returning this cached value if one exists. Otherwise
## this function calculates the inverse (it is assumed to exist), stores it as the new
## cached value, and returns it.

cacheSolve <- function(x, ...) {
    # get the possible cached inverse
    cached <- x$getInverse()
    if (!is.null(cached)) {
        # return it if it's not null
        cached
    } else {
        # otherwise, calculate the inverse and cache it
        inverse <- solve(x$get())
        x$setInverse(inverse)
        inverse
    }
}
