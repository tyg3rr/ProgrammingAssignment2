#' Creates a cache matrix object
#'
#' This function creates a cache matrix object that allows for efficient
#' computation of the inverse of a matrix. The cache matrix object stores
#' the original matrix and its inverse, if computed.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



#' Cache Solve
#'
#' This function calculates the inverse of a matrix and caches the result for future use.
#' If the inverse has already been calculated, the function will return the cached result.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}