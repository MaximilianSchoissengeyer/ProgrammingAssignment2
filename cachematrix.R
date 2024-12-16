## this function stores four functions in a list. These functions are used to set and get the matrix, and set or get the inverse of it.

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(n) {
    m <<- n
    inv <<- NULL
  }
  get <- function() m
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## this function calculates the inverse of the matrix and stores it in the cache.
cacheSolve <- function(m, ...) {
  inv <- m$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data, ...)
  m$setinv(inv)
  inv
}