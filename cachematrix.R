## Put comments here that give an overall description of what your
## functions do

## The function defines additional variable for inverse matrix in its scope
## It also returns a list of functions to encapsulate access to the original and inverse matrixes
makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  getInv <- function() invX
  setInv <- function(inverseX) invX <<- inverseX
  list(get = get, set = set, getInv = getInv, setInv = setInv)
}


## The function calls getInv()/setInv() of a special "matrix" object to get or set inverse matrix
cacheSolve <- function(x, ...) {
  invX <- x$getInv()
  if(!is.null(invX)) {
    message("getting cached data")
    return(invX)
  }
  xx <- x$get()
  invX <- solve(xx, diag(nrow(xx)), ...)
  x$setInv(invX)
  invX
}
