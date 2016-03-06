## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # function to set the matrix
  set <- function(y)   {
        x <<- y  }
  # function to get the matrix
  get <- function ()x
  
  # function to set the matrix
  setInverse <- function (inv){i <<- inv}
  # function to return the inverse of the matrix
  getInverse <- function ()i
  
  #retutn list of all function
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  ## get the matrix
  data <- x$get()
  ## Check if it is  square matrix
  if (nrow(data)==ncol(data)){
  i  <- solve(data, ...)}
  else { message("Unable to calculate inverse") }
  x$setInverse(i)
  i 
}
