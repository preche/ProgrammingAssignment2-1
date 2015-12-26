################ makeCacheMatrix ############################################################
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Set the value of the matrix
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  ### Get the value of the  matrix
  get <- function() x
  ### Set the value of the inv matrix to cached vales
  setInverse <- function(inverse) cachedInverse <<- inverse
  ### Get the value of the cached inv matrix
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
################ cacheSolve ##############################################################
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above.If the inverse has already been calculated (and the matrix has not changed), 
## then thecachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Check wheteher inverse of 'x' has been computed
  invFunc <- x$getInverse()
  if(!is.null(invFunc)) { ## inverse of 'x' is in cache
    message("getting cached data")
    return(invFunc)
  }
  ## otherwise compute inverse of 'x'
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
  invFunc
}
