## The makeCacheMatrix function computes the inverse of a matrix and 
## and computes the inverse of the matrix and also caches it for future use
## this prevents the unnecessary computing power if we are dealing with 
## large matrices

## the function computes the inverse of a matrix and gets the inverse.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse 
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## This function computes the inverse of the matrix returned by the
## makeCacheMatrix and if the inverse has aleady been calculated and the 
## matrix hasn't changed then this function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("retrieving cached data")
    return(inv)
    
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv

}
