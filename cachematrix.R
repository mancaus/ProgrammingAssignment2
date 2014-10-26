## Implements a "cache matrix" that caches the result of solve (matrix inverse)
## function.
##
## To use, construct a cache matrix via makeCacheMatrix, then use cacheSolve to
## get the (possibly cached) inverse.

## makeCacheMatrix 
##
## Return a list containing the following functions, which are used to work 
## with the cache matrix:
##   get - Get the matrix to be solved.
##   set - Set the matrix to be solved. If the inverse it set it will be cleared.
##   getinverse - Get the matrix inverse. This will be null if not yet set.
##   setinverse - Set the matrix inverse.
##
## Takes one optional argument - the matrix to calculate the inverse of. Default
## is the empty matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve
##
## Solve a cache matrix. Once computed the cached inverse will be returned.
##
## The first argument is the cache matrix. Remaining arguments are passed to 
## the solve function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
