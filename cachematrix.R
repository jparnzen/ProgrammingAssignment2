## 'makeCacheMatrix' creates a new cached matrix object, encapsulating
## the passed in matrix object, and returns a list object of functions
## that act as methods of the cached matrix object.

## NOTE that this follows the same pattern as was used in the
## assignment's makeVector() example.

## INTERFACE
## cm <- makeCacheMatrix(m) # creates a cached matrix from m with a NULL inverse
## cm$get() # returns the stored matrix
## cm$set(m2) # sets the stored matrix and resets the inverse to NULL
## cm$getInverse() # returns the inverse of the matrix (NULL if no inverse)
## cm$setInverse(im) # sets the inverse of the matrix

## EXAMPLE USAGE
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## h8 <- hilbert(8)
## cm <- makeCacheMatrix(h8)
## cm$get() # == h8
## cm$getInverse() # == NULL
## cm$setInverse(solve(h8))
## cm$getInverse() # == solve(h8)
## round(cm$getInverse() %*% cm$get(), 3) # == 8x8 identity matrix
## cm$set(hilbert(4))
## cm$get() # == hilbert(4)
## cm$getInverse() # == NULL

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


## 'cacheSolve' takes a cached matrix object and returns its inverse.
## If the inverse doesn't exist for the cached matrix, it is solve()ed
## and then stored in the cached matrix object.
## If the inverse does exist in the cached matrix, then the cached inverse
## will be returned and no new calculations are performed. A message also
## displays letting the user know that the cached inverse was used.
## Any additional arguments to 'cacheSolve' are passed to the solve() function.

## NOTE that this follows the same pattern as was used in the
## assignment's cachemean() example.

## EXAMPLE USAGE
## cm <- makeCacheMatrix(hilbert(8))
## im <- cacheSolve(cm)
## round(im %*% cm, 3) # == 8x8 identity matrix

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
  i <- solve(m, ...)
  ## cache the inverse for later
  x$setInverse(i)
  ## and return the inverse
  i
}
