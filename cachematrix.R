## makeCacheMatrix allows to associate a pre-computed representation of a matrix 
## inverse to the respective matrix object. It caches the result to avoid later 
## re-computing of the same inverse matrix.
##
## Usage example:
## set.seed(123)
## plain_matrix <- matrix(runif(n = 9), nrow = 3)
## x <- makeCacheMatrix()
## x$set(plain_matrix)
## x_inverse <- cacheSolve(x)
## all(x_inverse == solve(plain_matrix))

## makeCacheMatrix() provides functions to set, get and update an extended 
## matrix object, namely a CacheMatrix, in the main environment so the 
## CacheMatrix can be retrieved by other functions. To instantiate a CacheMatrix
## you need to call the set() method on the CacheMatrix object created.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) inverse <<- mean
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve just retrieve the cached inverse matrix of a CacheMatrix 
## if already computed; it computes the inverse matrix and caches it otherwise. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
