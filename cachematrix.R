## These functions cache potentially time-consuming computations
## allowing quick access to the result of a computation when required repeatedly
## for example in a looping operation of a large vector that remains unchanged.
## Putting the result in a cache means that the computation does not have to be
## repeated.

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(mean) m <<- mean
      getInverse <- function() m
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function computes the inverse of the matrix returned by 
## makeCacheMatrix above if it has not already been calculated.
## If the inverse has been calculated it retrieves the inverse from 
## the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      if(!is.null(m)) {
            message("Getting cached data!")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      m
}
