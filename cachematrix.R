## Below two funtions will solve matrix and cache matrix 
##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  
  get <- function() m
  get_inv <- function() inv
  set_inv <- function(x) inv <<- x 
  
  list(get=get, get_inv=get_inv, set_inv=set_inv)
}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  if (!is.null(m$get_inv())) {
    print("getting cached data")
    return(m$get_inv())
  }
  m$set_inv(solve(m$get()))
  return(m$get_inv())
  
}

m = makeCacheMatrix(matrix(rexp(200), ncol=4, nrow=4))
cacheSolve(m)
cacheSolve(m)