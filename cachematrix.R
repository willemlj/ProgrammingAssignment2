## R Programming, assignment 2 - Lexical Scoping
## Author: Willem Joosten
## Date: 27 december 2015

## Create an 'object' for a matrix which can keep a cached copy of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## the calculated inverse or NULL when the inverse has not been
  cachedResult <- NULL

  ## set the matrix, will also reset the cached value  
  set <- function(newMatrix) {
    x <<- newMatrix
    cachedResult <<- NULL
  }
  
  ## get the matrix
  get <- function(){
    x 
  }
  
  ## set the inverse
  setInverse <- function(newInverse){
    cachedResult <<- newInverse
  }
  
  ## get the inverse
  getInverse <- function(){
    cachedResult
  }
  
  ## return list of all functions
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## solve the given matrix, if the solution has already been calculated use the copy 
## of the calculation instead of calculating it again.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(! is.null(inverse)){
    message("Getting cached inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
