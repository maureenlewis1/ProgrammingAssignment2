## These functions reduce time-consuming computations involving taking 
## the inverse of a large invertible matrix, instead allowing the inverse to be 
## retrieved from the cache, rather than continuously calculated, if already determined.

## create a matrix and calculate its inverse

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## check if inverse has been calculated, if it has, retrieve from cache. If it has not, then it will calculate the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
