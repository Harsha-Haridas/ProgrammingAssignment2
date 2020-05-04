## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
 }


## cacheSolve is a function that computes the inverse of special "matrix" returned by the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("received a cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix,...)
  x$setInverse(inv)
  inv
}

