## Matrix inversion is usually time-consuming. There may be some benefits in caching the inverse of a matrix ## rather than computing it repeatedly. These two functions below help to cache the inverse of a matrix.

## The function makeCacheMatrix() is used to create a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  ## x is a square invertible matrix. This will return a list containing function to:
  ##        1. set the matrix
  ##        2. get the matrix
  ##        3. set the inverse of the matrix
  ##        4. get the inverse of the matrix
  
  # Initially set to NULL
  inv <- NULL
  
  # Set the matrix
  set <- function (y) {
    # '<<-' is used to assign a value to an object in an environment different from the current environment.
    x <<- y
    inv <<- NULL
  }
  
  # Get the matrix
  get <- function () x
  
  # Set the inverse of the matrix
  setinv <- function (inverse) inv <<- inverse
  
  # Get the inverse
  getinv <- function () inv
  
  # Put into a list
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function cacheSolve() is used to compute the inverse of the special "matrix" returned by
## makeCacheMatrix() above. If the inverse has already been calculated (and the matrix has not changed), then ## the cacheSolve()  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## x is the output of function makeCacheMatrix()  
  ## Return the inverse of the original matrix inputted to makeCacheMatrix()
  
  # Get the current state of the inverse and see if it has already been calculated
  inv <- x$getinv()
  
  # If the inverse has already been calculated:
  if (!is.null(inv)) {
    # get it from the cache and skip it from the computation
    message ("Getting cached matrix")
    return (inv)
  }
  
  # Otherwise, calculate the inverse:
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse in the object
  x$setinv (inv)
  return (inv)
}