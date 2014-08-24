## This routine takes as input a matrix from the user
## and creates a special object that can optimize matrix
## inversion by caching results.
## Example: sqmar
## sqmat <- matrix(4:7,2,2)
## cachedmat <- makeCacheMatrix(sqmat)
## cacheSolve(cm)

## This routine takes a matrix as input and exposes 
## routines which allow cacheSolve to fetch cached
## inverted matrix copy, if it already exists.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
}

## This routine takes as input a special "cachedMatrix"
## and returns a inverted one

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached matrix inversion data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

