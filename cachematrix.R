## 2 functions below allow you to cache the inverse of a matrix rather than repeatedly 
## calculating it
##This first function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs <<- inverse
  getinverse <- function() invs
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse or retrieves the cached inverse if it has already
## been solved
cacheSolve <- function(x, ...) {
  invs <- x$getinverse()
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data, ...)
  x$setinverse(invs)
  invs
}