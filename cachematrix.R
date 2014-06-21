## This file contain two function that create "special" matrix object, 
## computing its inverse and set its inverse in the cache 

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function (y) {
    x <<- y
    minv <<- NULL
  }
  get <- function () x
  setMinv <- function (matinv) minv <<- matinv
  getMinv <- function () minv
  list(set=set, get=get, setMinv=setMinv, getMinv=getMinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getMinv()
  if (!is.null(minv)) {
    message("Getting Cached data")
    minv
  }
  data <- x$get()
  matinv <- solve(data)
  x$setMinv(matinv)
  matinv
}
