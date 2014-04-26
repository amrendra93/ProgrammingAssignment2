## Matrix inversion is usually a repeatitive and time consuming process.
## The aim of fucntions are to creates a special "matrix" object that can cache its inverse.
## Then it computes the inverse of the special "matrix" returned,  if the inverse has already been calculated
## and input matrix data has not changed, then it retrieve the inverse from the cache.
 
## Assumption: Input marix should be square matrix to compute its inverse without error.

## makeCacheMatrix creates a special "matrix", which is really a list containing four fucntions 
## input for second function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse matrix
  setInv <- function(solve) m <<- solve
  
  ## Solve and get the value of the inverse matrix
  getInv <- function() m
  
  ## Create output list for second fucntion
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve calculates the inverse of the input Matrix created 
## with the above function and returns the nverse of the input matrix. But first checks
## to see if the inverse has already been calculated and data has not changed, if yes, 
## then skips the computation and returns inverse matrix from cache saved by first fucntion.

cacheSolve <- function(x, ...) {
  ## Look for cache data and return inverse matrix, if found
cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Return a matrix that is the inverse of 'x', if input matrix data has changed
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
