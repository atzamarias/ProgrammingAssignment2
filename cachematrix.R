## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Cache a matrix
makeCacheMatrix <- function(mtrx = matrix()) {
    
    invmtrx <- NULL
    
    setmatrix <- function(y) {
      mtrx <<- y
      invmtrx <<- NULL
    }
    
    getmatrix <- function() {
      mtrx
    }
    
    setinvmatrix <- function(matrix) {
      invmtrx <<- matrix
    }
    
    getinvmatrix <- function() {
      invmtrx
    }
    
    list(setmatrix = setmatrix, getmatrix = getmatrix, setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)
}


## Write a short comment describing this function
## Inverse a square matrix
cacheSolve <- function(x, ...) {
  invmatrix <- x$getinvmatrix()
  
  if(!is.null(invmatrix)) {
    message("Getting cached data...")
    return(invmatrix)
  }
  
  calcmatrix <- x$getmatrix()
  calcmatrix <- solve(calcmatrix, ...)
  x$setinvmatrix(calcmatrix)
  calcmatrix
}
