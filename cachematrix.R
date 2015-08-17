## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## the private cached inverse property, initialized to NULL
  inv <- NULL
  
  ## the object methods for setting and getting the matrix
  ## and its cached inverse:
  
  ## 'set' copies the provided matrix into the object
  ## and resets the cached inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## 'get' returns the object's matrix
  get <- function() x
  
  ## 'setInverse' copies the provided inverse into
  ## the object's cached inverse property
  setInverse <- function(i) inv <<- i
  
  ## 'getInverse' returns the current cached inverse
  getInverse <- function() inv
  
  ## set up our object's public interface as a named list of functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## first, check if we have a cached inverse
  i <- x$getInverse()
  ## if the inverse isn't NULL, then we have 
  ## a cached result and we can just return it
  if (!is.null(i)) {
    message("Using cached inverse...")
    return(i)
  }
  
  ## otherwise, calculate the inverse and cache it:
  
  ## get the internally stored matrix
  m <- x$get()
  ## create the inverse
  i <- solve(m)
  ## cache the inverse for later
  x$setInverse(i)
  ## and return the inverse
  i
}
