## Functions for caching the inverse of a square matrix

## makeCacheMatrix() function:
##  creates a list containing functions to:
##  - set(ie, caches) the value of the matrix
##  - get the value of the matrix
##  - set(ie, caches) the inverse of the matrix
##  - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve() function:
##  returns the cached inverse matrix from makeCacheMatrix list;
##  if no inverse matrix cached, then calculate inverse and store result
##  in makeCacheMatrix list

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
          message("getting cached data")
          return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
