## Below are two functions that are used to create a special object 
## that stores a matrix and cache's its inverse. 

## This function creates a special "matrix" object that can cache its inverse
## Matrix inversion is usually a costly computation and so its benefitial
## to cache the inverse of a matrix rather than compute it repeatedly

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve(x) %*% x
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above. If the inverse has already 
##  been calculated (and the matrix has not changed), then the 
##  cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
