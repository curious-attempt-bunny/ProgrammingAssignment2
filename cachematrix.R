## Cache solving a matrix for its inverse.
## Example usage:
## cacheMatrix <- makeCacheMatrix(
##                  matrix(c(1, 5, 2, 4, 1, 2, 7, 8, 9), 3, 3)
##                )
## inverse1 <- cacheSolve(cacheMatrix)
## inverse2 <- cacheSolve(cacheMatrix)
## # inverse2 will be the cached version
##
## To reuse a cacheMatrix with a new matrix, you can use cacheMatrix$set(...).
##
## Note that these functions assume that the matrix can be solved without errors.


## Make a new cacheMatrix from a matrix. Intended for use with the cacheSolve function.
## By convention, the cacheMatrix has two functions intended for public use:
## cacheMatrix$set(...), and cacheMatrix$get() to set and get the matrix being cached.
##
## Note that calling cacheMatrix$set(...) will erase any cached solution for the matrix.

makeCacheMatrix <- function(wrappedMatrix = matrix()) {
  cachedInverse = NULL
  set <- function(newMatrix) {
    wrappedMatrix <<- newMatrix
    cachedInverse <<- NULL
  }
  get <- function() wrappedMatrix
  setInverse <- function(newInverse) cachedInverse <<- newInverse
  getInverse <- function() cachedInverse
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## A cached implementation of solve(...) that takes a cacheMatrix as the first argument,
## and arguments to solve as the remaining arguments.
##
## See makeCacheMatrix(...) for details on making a cacheMatrix.
## 
## Note that this functions assumes that the matrix can be solved without errors.

cacheSolve <- function(cacheMatrix, ...) {
  cachedInverse <- cacheMatrix$getInverse()
  if(!is.null(cachedInverse)) {
    message("getting cached inverse")
    return(cachedInverse)
  }
  wrappedMatrix <- cacheMatrix$get()
  cachedInverse <- solve(wrappedMatrix, ...)
  cacheMatrix$setInverse(cachedInverse)
  cachedInverse
}
