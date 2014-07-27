## Coursera rprog-005
## Programming Assignment 2: Create a "matrix"-like object and provide
## getters and setters, as well as a method to cache the inverse of the matrix
## since this is an expensive operation.


## This function creates a "matrix"-like object
## which can cache the results of the solve() operation
## (which find the inverse of the matrix).
makeCacheMatrix <- function(x = matrix()) {
  # member variables
  m_inverse <- NULL
  
  # get this matrix object
  get <- function() x
  
  # get the inverse of this matrix,
  # or NULL if it hasn't been calculated yet
  getInverse <- function() m_inverse
  
  # set this matrix object to the given argument,
  # resetting all member variables
  set <- function(x0) {
    x <<- x0
    m_inverse <<- NULL
  }
  
  # set the inverse of this matrix object
  setInverse <- function(inverse) m_inverse <<- inverse
  
  # return the object
  list(get = get, set = set,
       getInverse = getInverse,
       setInverse = setInverse)
}


## Returns the inverse of the given matrix. 
## This function caches the result in the "matrix" object if needed.
## (If the matrix already knows its inverse, then simply
## return the cached value instead of recalculating.)
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  # get the cached data if it exists
  if (!is.null(inverse)) {
    message("Getting cached data.")
    return (inverse)
  }
  
  # otherwise, calculate the inverse of the matrix...
  data <- x$get()
  inverse <- solve(data, ...)
  
  # ...cache the result...
  x$setInverse(inverse)
  
  # ...and finally return the inverse
  inverse
}


## Test the code.
test <- function() {
  mat <- matrix(rnorm(25), 5, 5)
  
  # generate the expected result
  matInv <- solve(mat)
  
  # generate the matrix object
  cacheMat <- makeCacheMatrix(mat)
  
  # test for equality on non-cached data
  non_cached_equal = all(matInv == cacheSolve(cacheMat))
  stopifnot(non_cached_equal)
  message("non_cached_equal OK!")
  
  # test for equality on cached data
  cached_equal = all(matInv == cacheSolve(cacheMat))
  stopifnot(cached_equal)
  message("cached_equal OK!")
  
  # print success message
  message("All tests OK!")
}
