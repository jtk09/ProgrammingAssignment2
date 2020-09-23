## Combined, the makeCacheMatrix and cacheSolve functions compute the inverse
## of a given matrix and store it in cache for retrieval should the inverse 
## of the given matrix be needed again.

## the makeCacheMatrix follows a structure identical to the example 
## makeVector function. makeCacheMatrix produces a list of 4 functions: 
## 1) "set" assigns the matrix to a locally (within the function environment) 
## stored variable, 2) "get" gets the matrix, 3) "setinv" assigns the computed 
## inverse of the matrix, and 4) retrieves the matrix inverse
## ex: varname = makeCacheMatrix(matrix(A)) with A a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve is where the matrix inverse is computed, should it not already be 
## cached. On the first call cacheSolve computes the inverse and calls
## setinv() within makeCacheMatrix to store the computation. A second call retrieves the
## stored matrix through the call to getinv() of makeCacheMatrix rather than re-computing 
## A's inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
