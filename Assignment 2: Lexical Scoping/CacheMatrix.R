## These functions provide a way to store and retrieve the inverse of a matrix,
## potentially saving computational time by avoiding repeated inversions when the matrix remains unchanged.


## Creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
  
  getInverse <- function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes and caches the inverse of a matrix, retrieving the cached inverse if available.

cacheSolve <- function(x, ...) {
  
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Retrieving cached inverse...")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  
  inverse
}
