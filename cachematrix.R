## ## Matrix inversion is usually is time consuming process, 
## The aim of fucntions is to creates a special "matrix" object that can cache its inverse.
## Then it computes the inverse of the special "matrix" returned,  if the inverse has already been calculated
## and input matrix data has not changed, then it retrieve the inverse from the cache.
 
## Assumption: Input marix should be square matrix to compute its inverse without error.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Look for cache data and return inverse matrix, if found
cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Return a matrix that is the inverse of 'x', if input matrix data changed
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
