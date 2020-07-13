## The makeCacheMatrix function computes the inverse of a matrix and 
## and computes the inverse of the matrix and also caches it for future use
## this prevents the unnecessary computing power if we are dealing with 
## large matrices

## the function computes the inverse of a matrix and gets the inverse.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    
    x <<- y   # The double arrow assignment operator is a closure on the
              # function environment. It instructs R to look for the value 
              # of floating variables.
    
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function() inv <<- inverse 
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## The cacheSolve function computes the inverse of the matrix returned by the
## makeCacheMatrix and if the inverse has aleady been calculated and the 
## matrix hasn't changed then this function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse() #this command gets the inverted square matrix x if it
                        #is calculated and stored by the above makeCachematrix.
  
  if(!is.null(inv)) {  # This is a not operator asking that if inv is not null
                       # then please return the inverted matrix.
    
    message("retrieving cached data")
    
    return(inv)
    
  }
  
  # the codes written below perform the inverting of matrix if the solution
  # is not cached and return the inv matrix.
  mat <- x$get()
  inv <- solve(mat, ...) # solve() is a built in fution in R for calculating 
                         # the inverse of a matrix.
  x$setInverse(inv)
  inv

}
