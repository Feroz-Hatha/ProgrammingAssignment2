## Below are two functions that are used to create a 
## special object that stores a matrix (assumed to be
## invertible) and cache's its inverse.

## The following function, makeCacheMatrix creates a  
## special "vector", which is really a list containing  
## functions to set the value of the matrix, get the value 
## of the matrix, set the value of the inverse, and get  
## the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) {i <<- inverse}
  getinverse <- function() {i}
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the 
## matrix returned in the special "vector" created using  
## the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets  
## the inverse from the cache and skips the computation.  
## Otherwise, it calculates the inverse of the matrix and  
## sets the value of the inverse in the cache via the 
## setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setinverse(i)
  i
}
