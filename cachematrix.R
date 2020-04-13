## A set of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

# initialize the inverse matrix
  inverse <- NULL
  
  # function to set matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }

  # function to get matrix
  get <- function() x
  
  # function to set inverse matrix
  setInverse <- function(i) inverse <<- i
  
  # function to get inverse matrix
  getInverse <- function() inverse
  
  #Return the list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
# Return inverse of matrix x
  inverse <- x$getInverse()
  
  # Return the inverse if it is already set
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # Get the matrix and calculate its inverse value
  data <- x$get()
  inverse <- solve(data) %*% data
  
  # Set calculated inverse value to the object
  x$setInverse(inverse)
  
  #Return the inverse matrix
  inverse
}
