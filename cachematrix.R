## Put comments here that give an overall description of what your
## fx will do

## Fx that creates matrix object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize inverse property
    inv <- NULL

  ## Method to set matrix
  set <- function(y) {
    matrix <<- y
    inv <<- NULL
  }
  
  ## Method to get matrix
  get <- function(){
    ## return matrix
    matrix()
  }
  
  ##Method to set inverse of matrix
  setInverse <- function(inverse) {
    ## storing inverse
    inv <<- inverse
    
  }
  
  ## Returns list of methods
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}
  
  
## Computes the inverse of the special matrix returned by the 
## "makeCacheMatrix" fx 
## If the inverse has already been calculated (and the matrix has not 
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Getting a matrix that is the inverse of x
  inv <- x$getInverse()
    
  ## Returns if the inverse has already been calculated
  if(!is.null(inv)) {
    message("Getting cached data.")
    return(inv)
  }
  
  ## If the inverse has not been calculated
  
  ## Getting the matrix from our object
  data <- x$get()
  
  ## Calculating the inverse by using matrix multiplication
  m <- solve(data) %*% data
  
  ## Storing the inverse to the object for future usage
  x$setInverse(m)
}  


