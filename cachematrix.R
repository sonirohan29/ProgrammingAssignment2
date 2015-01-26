# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInv   get the cached value (inverse of the matrix)
# * getInv     get the cached value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
  # input the matrix  
  #cache shall hold the cached value
  # initially nothing is cached so set it to NULL
  cache <- NULL
  # store a matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    # since the matrix is assigned a new value, flush the cache
    cache <<- NULL
  }
  
  # returns the stored matrix
  getMatrix <- function() {
    x
  }
  
  # cache the given argument 
  cacheInv <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  getInv <- function() {
    cache
  }
  
  # return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInv = cacheInv, getInv = getInv)
}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(y, ...) {
  # get the cached value
  inverse <- y$getInv()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  else{
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  d <- y$getMatrix()
  inverse <- solve(d)
  #solve function calculates inverse
  y$cacheInv(inverse)
  
  # return inverse
  return(inverse)
  }
}