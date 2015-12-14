## Description:
  ## store a matrix and its inverse in cache to avoid recalculation

## makeCacheMatrix: 
  ## defines functions for setting/getting the matrix and its inverse in cache
  ## returns list of these functions

makeCacheMatrix <- function(m = matrix()) {
  ## init mi: matrix inverse
    mi <- NULL 

  ## store matrix to cache
      set <- function(y) {
      m <<- y
      mi <<- NULL
    }

  ## return matrix from cache
    get <- function() m

  ## store inverse to cache
    setInverse <- function(inverse) mi <<- inverse

  ## return inverse from cache    
    getInverse <- function() mi

  ## return list of functions  
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve:
  ## retrieves the matrix inverse from cache, or if NULL solves and stores the inverse
  ## returns inverse
  
cacheSolve <- function(m, ...) {
  ## get matrix inverse from cache
    mi <- m$getInverse()
    if(!is.null(mi)) {
      message("getting cached matrix inverse")
      return(mi)
    }

  ## cache is NULL, so solve for matrix inverse
    tempMatrix <- m$get()
    mi <- solve(tempMatrix) ## assignment assumes matrix is always invertible
    m$setInverse(mi)
    mi
}


