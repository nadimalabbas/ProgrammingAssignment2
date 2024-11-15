## Return a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)
}

## Retrieve the inverse of x from cache or calculate it if it's NULL
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return (inv)
  }
  inv <- solve(x$get(), ...)
  x$setInverse(inv)
  inv
}

