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
