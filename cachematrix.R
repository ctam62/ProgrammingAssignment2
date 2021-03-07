## The following functions are an example of how to cache potentially
## time-consuming computations (e.g. calculating the inverse of a matrix)
## using the scoping rules of the R language.


## makeCacheMatrix creates a special "matrix" object that 
## can cache its inverse.
## Example usage: mat <- makeCacheMatrix()
##                mat$get()
##                mat$getInverse()
##                mat$set(matrix(rnorm(16), 4, 4))
##                mat$setInverse(solve(mat$get()))

makeCacheMatrix <- function(x = matrix()) {
  ## Return a special "matrix" object of class type list that contains a list
  ## of functions to set the value of a matrix, get the value of a matrix, set
  ## the inverse of a matrix, and get the inverse of a matrix
  
  inv <- NULL
  
  # function to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # function to get the value of the matrix
  get <- function() x
  # function to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  # function to get the inverse of the matrix
  getInverse <- function() inv
  # function list
  fun_list <- list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
  # return the function list
  return(fun_list)
}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should retrieve
## the inverse from the cache.
## Example usage: cacheSolve(mat)

cacheSolve <- function(x, ...) {
  ## x is a special "matrix" object of class type list
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  
  # check if the inverse has already been calculated
  if(!is.null(inv)) {
    message("getting cached data")
    # return cached inverse
    return(inv)
  }
  
  # get the matrix data from x
  data <- x$get()
  # compute the inverse of the matrix
  inv <- solve(data, ...)
  # set the inverse of the special "matrix" object x
  x$setInverse(inv)
  
  # return the inverse of the matrix
  return(inv)
}
