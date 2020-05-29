## Put comments here that give an overall description of what your
## functions do
## This function creates a special "matrix" object that can cache its inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## @x: a invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  matriz <- NULL
  set <- function(y) {
      x <<- y
      matriz <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) matriz <<- inverse
  getInverse <- function() matriz
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
 

## Write a short comment describing this function
# it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the mean 
# in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  ## @x: a invertible matrix
  ## Return a matrix that is the inverse of 'x'
  inv = x$getInverse()
  
  if (!is.null(inv)){
    message("getting cached matrix")
    return(inv)
  }
  
  matriz = x$get()
  inv = solve(matriz, ...)
  
  x$setInverse(inv)
  
  return(inv)
}
