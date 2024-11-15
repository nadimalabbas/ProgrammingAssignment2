## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL               ## value of inverse matrix, initialized as NULL
  set <- function(y) {    ## define the function that sets the matrix
    x <<- y               ## change value of matrix in parent environment
    m <<- NULL            ## if matrix is changed, reset its cached inverse to NULL
  }
  get <- function() x     ## define the function that gets the value of the matrix
  setInverse <- function(inv) m <<- inv ## define the function that sets the new cached inverse in parent environment
  getInverse <- function() m            ## define the function that gets the cached inverse
  
  ## Return a list of the setters and getters of the matrix and its cached inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Retrieve the inverse of the matrix x from cache, or calculate it if it hasn't
## been calculated before (if the inverse is NULL)
## As per the assignment instructions, it is assumed that the matrix supplied is
## always invertible
cacheSolve <- function(x, ...) {
  m <- x$getInverse()     ## get the inverse from the cache and store it in m
  if(!is.null(m)) {       ## check if there is a cached value in m 
    ## if there is a value stored in m, i.e, inverse has been computed before
    ## then return this cached value
    message("getting cached inverse")
    return (m)
  }
  ## if we reach this line then the function didn't return, which means m is
  ## NULL, which means there is no cached value stored, so we need to compute it
  data <- x$get()       ## get the value of the matrix and store it in `data`
  m <- solve(data, ...) ## compute the inverse of the matrix `data` and store in `m`
  x$setInverse(m)       ## store this computed inverse `m` in the cache
  m                     ## return the computed inverse `m`
}

