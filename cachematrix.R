## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inver <<- inverse
  get_inverse <- function() inver
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$get_inverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  matrix <- x$get()
  inver <- solve(matrix, ...)
  x$set_inverse(inver)
  inver
}
