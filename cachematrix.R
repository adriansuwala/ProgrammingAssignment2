## Functions that allow caching matrix inverse until the matrix changes,
## without any additional bookkeeping from a user.

## Returns a list of functions that allow changing/retrieving given matrix
## and its inverse.
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


## Returns inverse of a matrix x (assumes non-singularity) from cache if saved
## and x didn't change since computation. Otherwise recomputes inverse and saves it.

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
