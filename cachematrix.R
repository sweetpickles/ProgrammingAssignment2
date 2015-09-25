## This program includes an object constructor and methods for caching the inverse
## of a matrix. It allows retention of the inverse of the matrix along with the matrix
## to potentially prevent repeated computation of the inverse.

## This function creates a makeCacheMatrix object with get/set methods for the matrix
## and get/set methods for solving the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL

  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  get <- function()
    x

  setinverse <- function(inv)
    i <<- inv

  getinverse <- function()
    i

  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if the inverse is cached and computes if it is not

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
