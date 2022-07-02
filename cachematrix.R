## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Cache a matrix
makeCacheMatrix <- function(mtrx = matrix()) {
    ##Reset the matrix
    invmtrx <- NULL
    
    ##Set the matrix and reset the inverse matrix variables
    setmatrix <- function(y) {
      mtrx <<- y
      invmtrx <<- NULL
    }
    
    ##Get the matrix
    getmatrix <- function() {
      mtrx
    }
    
    ##Set the inverse matrix
    setinvmatrix <- function(matrix) {
      invmtrx <<- matrix
    }
    
    ##Get the inverse matrix
    getinvmatrix <- function() {
      invmtrx
    }
    ##Create and return the list that contains the functions
    list(setmatrix = setmatrix, getmatrix = getmatrix, setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)
}


## Write a short comment describing this function
## Inverse a square matrix
cacheSolve <- function(x, ...) {
  ##Try to get the cached matrix
  invmatrix <- x$getinvmatrix()
  
  ##check if it is cached
  if(!is.null(invmatrix)) {
    message("Getting cached data...")
    return(invmatrix)
  }
  
  ##Otherwise inverse the given matrix
  calcmatrix <- x$getmatrix()
  calcmatrix <- solve(calcmatrix, ...)
  x$setinvmatrix(calcmatrix)
  calcmatrix
}
