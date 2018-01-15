## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv_x <<- inverse
  getInv <- function() inv_x
  list(set=set, get=get, setInv=setInv, getInv=getInv)
  
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse from 
# the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getInv()
  if (!is.null(inv_x)) {
    message("Getting the cached data")
    return(inv_x)
  }
  
  data<-x$get()
  inv_x<-solve(data, ...)
  x$setInv(inv_x)
  inv_x
}
