## Put comments here that give an overall description of what your
## functions do
## The pair is used to calculate the inverse of the matrix and store it in the cahce
## on the call it will first check the cahce  if the inverse exists and return that value
## instead of re-calcuting else it will calculate the inverse

## Write a short comment describing this function
## This function will set and get the value of the matrix and it's inverse 
##and also store it chache

makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) mat_inv <<- inverse
  getInverse <- function() mat_inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## Calculates the inverse of the matix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  mat_inv <- x$getInverse()
  
  ##check if the inverse available in cache
  if (!is.null(mat_inv)) {
    message("Getting the cached matrix")
    return(mat_inv)
  }
  
  mat_org <- x$get()
  mat_inv <- solve(mat_org, ...)
  x$setInverse(mat_inv)
  mat_inv
}
