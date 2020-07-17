## Functions that define wrapper around a matrix to allow caching matrix inverse
## until the matrix changes, without any additional bookkeeping from a user.

## Wraps given matrix in an object that can cache inverse of the matrix and
## exposes functions to operate on the matrix and its inverse.
## `get` returns the matrix
## `set` sets matrix and invalidates cache
## `setinv` saves given matrix as inverse
## `getinv` returns saved inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        # matrix changed, invalidate cache
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(get = get,
         set = set,
         getinv = getinv,
         setinv = setinv)
}


## Returns an inverse of a matrix (assumes non-singularity)
## that is stored inside the wrapper x.
## Retrieves cached inverse if it was computed and matrix didn't change since.
## Otherwise recomputes inverse and saves it in the wrapper.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()

    # Happy path, if inverse is cached then nothing to compute
    if(!is.null(inv)) {
        message("reading from cache")
        return(inv)
    }
    # compute inverse and save in cache
    inv <- solve(x$get(), ...)
    x$setinv(inv)
    inv
}
