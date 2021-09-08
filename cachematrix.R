## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(temp = matrix()) 
{
  inverse <- NULL
  set <- function(temp2)
  {
    temp <<- temp2
    inverse <<- NULL
  }
  get <- function() temp
  f_setInverse <- function(solveMatrix) inverse <<- solveMatrix
  f_getInverse <- function() inverse
  list(set = set, get = get, f_setInverse = f_setInverse, f_setInverse = f_setInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(temp, ...) 
{
  inverse <- temp$f_getInverse()
  if(!is.null(inverse))
  {
    message("Received cached data!")
    return(inverse)
  }
  data <- temp$get()
  inverse <- solve(data)
  temp$f_setInverse(inverse)
  inverse      
}
