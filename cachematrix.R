## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a vector that contains functions to interact with a matrix and a
## value for it's inverse
## get - Get stored matrix
## set - Store a new matrix
## getInverse - Get the (if already computed) inverse of the provided matrix
## setInverse - Stores a secondary matrix (intended for solve() output caching)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(matrix) m <<- matrix
  getInverse <- function() m
  #build vector containing function mappings
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## Calculates the inverse of the matrix stored in the makeCacheMatrix vector
## and stores (caches) the output to save computation time on subsequent calls

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  #do we already have it?
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #cached data was not found, calculate and store inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
