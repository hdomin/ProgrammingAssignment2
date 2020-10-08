## These functions are used to solve and save in chache the inverse of
## a matrix

## makeCacheMatrix creates a special "matrix", this matrix containing
## functions to set, get and solve the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setsolve <- function(solve) m <<- solve
  
  getsolve <- function() m
  
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The following function get from cache or resolve the inverse of the 
## special matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  
  if(!is.null(m)) {
    message("getting cache data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  
  m
}