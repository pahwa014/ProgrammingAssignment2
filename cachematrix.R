## Use the below methods for the caching and using the inverse of a matrix
## First enable the caching by using matrixCacheMatrix 
## Then use cacheSolve to get the inverse of the matrix. If cache available
## then the cache inverse is supplied.

## Methods for caching the matrix 

makeCacheMatrix <- function(x = matrix()) {
  cached <- NULL
  
  set <- function(y) {
    x <<- y
    cached <<- NULL
  }
  
  get <- function() {
    x
  }
  
  set.inverse <- function(inverse) {
    cached <<- inverse
  }
  
  get.inverse <- function() {
    cached
  }
  
  list(get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


## Solves a matrix and cache the result, in order to retrieve and reuse it later

cacheSolve <- function(x, ...) {
  inverse <- x$get.inverse()
  if(is.null(inverse)) {
    inverse <- solve(x$get(), ...)
    x$set.inverse(inverse)
  }
  inverse
}
