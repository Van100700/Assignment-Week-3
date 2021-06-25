## These series of functions are submitted as a partial fulfillment of R Programming in Coursera.

## This function forms a special "matrix" object which will cache its corresponding inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) s <<- inverse
  getinverse <- function() {inv}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function solves the said inverse of the special "matrix".
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
