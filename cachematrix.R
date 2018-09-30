## The goal of these functions is to save time when computing inverse matrix.
## Function use caching matrix instead of its repeatedly computing.

## makeCacheMatrix is a list with function to:
## set and get the value of the vector
## set and get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    a <- NULL
    set <- function(y) {
      x <<- y
      a <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) a <<- inverse
    getinverse <- function() a
    list(set = set,
      get = get,
      setinverse = setinverse,
      getinverse = getinverse)
  }

## cacheSolve returns inverse matrix from cache (if inverse matrix has been already calculated)
## or calculate inverse matrix (if it has not been calculated yet)

cacheSolve <- function(x, ...) {
    a <- x$getinverse()
    if (!is.null(a)) {
      message("getting cached data")
      return(a)
    }
    data <- x$get()
    a <- solve(data, ...)
    x$setinverse(a)
    a
  }
