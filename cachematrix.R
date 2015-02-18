## The functions below allow caching the inverse of a matrix,
## creating a special "matrix" object that has this capability.
## Matrix inversion is usually a costly computation and there is some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly

## 'makeCacheMatrix(x)': This function creates a special "matrix" object that
## can cache a matrix and its inverse
## 'x' is the original matrix
## 'makeCacheMatrix$set(x)' sets a new value for the matrix, and nullifies the value of the inverse
## 'makeCacheMatrix$get()' returns the stored matrix
## 'makeCacheMatrix$setInv(y)' sets the inverse of the stored matrix
## 'makeCacheMatrix$getInv()' returns the value of the stored matrix inverse, or NULL

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


## 'cacheSolve(x)': This function computes and returns the inverse of the special
## "matrix" returned by `makeCacheMatrix`. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve(x)` will retrieve the inverse from the cache.
## 'x' is the original `makeCacheMatrix` object where the inverse is cached


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
