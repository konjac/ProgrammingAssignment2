## The following two functions:
## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(x){
    m <<- x
    inv <<- NULL
  }
  get <- function() m
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the matrix above.

cacheSolve <- function(x, ...){
  ## inv is the variable to store the inverse
  inv <- x$getinv()
  if(is.null(inv)){
    ## if inv is null, compute and cache it
    message("computing and caching data")
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
  }else{
    message("getting cached data")
  }
  ## Return a matrix that is the inverse of 'x'
  inv
}
