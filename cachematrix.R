## Put comments here that give an overall description of what your
## functions do

## This function creates a "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInv <- function(inv) i <<- inv
    getInv <- function() i
    list (set = set, get = get, setInv = setInv,
          getInv = getInv)
}

## This function computes the inverse of the 
## matrix returned from "makeCacheMatrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)}
  mat <- x$get()
  i <- solve(mat, ...)
  x$setInv(i)
  i
  }
