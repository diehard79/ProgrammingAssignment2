# Function-  makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the inverse matrix
# - get the value of the inverse matrix
## Function to create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    #Set values
    x <<- y
    m <<- NULL  #Reset inverse matrix so that it is recalculate if matrix is changed
    
    
  }
  get <- function() x

  setMatrixInverse <- function(inverse) m <<- inverse
  getMatrixInverse <- function() m

    list(set = set, get = get,
         setMatrixInverse=setMatrixInverse,
         getMatrixInverse=getMatrixInverse
       )
}

## Function to compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrixInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrixInverse(m)
  m
}
