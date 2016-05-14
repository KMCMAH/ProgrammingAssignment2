## makeCacheMatrix - a function that creates a special matrix object that can cache its inverse
## cacheSolve is a function that calculates the inverse of the special matrix returned by
## makeCacheMatrix. If it's already been computed (and if it hasn't changed as well),  
## then cacheSolve will retrieve the inverse from the cache.

## 

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set= function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Output 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  ## check to see if it has already been computed
  if (!is.null(inv)){
    message("Ok, using cached data")
    return(inv)
    
  }
  # since not, calculate the inverse
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}
