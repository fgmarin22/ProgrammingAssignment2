## The functions below allow caching the inverse of a matrix,
## creating a special "matrix" object with this capability.
## Matrix inversion is usually a costly computation and there is some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly

## 'makeCacheMatrix': This function creates a special "matrix" object that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInv <- function(y) Inv <<- y
  getInv <- function() Inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## 'cacheSolve': This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix`. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data)
  x$setInv(Inv)
  Inv
}
