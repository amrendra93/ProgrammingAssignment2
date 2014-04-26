## function, makeVector creates a special "vector", which is really a list containing a function to 
## functions d

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
