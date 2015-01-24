## These functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # The m variable contains the special "matrix" over which the inverse was computed
  m <- NULL

  # The minverse variable contains the inverse of the special "matrix"
  minverse <- NULL
  
  # This function change the value of the current special "matrix", it is not
  # necesarialy which the inverse was computed 
  set <- function(y) {
    x <<- y
  }
  
  get <- function() x
  getm <- function() m
  
  # This function cache the value of a special matrix and its inverse
  setinversematrix <- function(inversematrix) {m <<- x; minverse <<- inversematrix}
  
  # This function returns the inverse cached of the a special matrix
  getinversematrix <- function() minverse
  list(set = set, get = get, getm = getm,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
  }


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # The variable m is set with the special "matrix" over which the inverse was computed
  m <- x$getm()

  # The variable inversem is set with the inverse of m (the previous variable)
  inversem <- x$getinversematrix()
  
  # The variable data is set with the current special "matrix".
  data <- x$get()
  
  # If the inverse has already been calculated and the matrix over which the inverse 
  # was computed has not changed then retrieves the inverse from the cache
  if(!is.null(m) && identical(m, data)) {
    message("getting cached data")
    return(inversem)
  }
  
  # Computes and set the inverse
  inversem <- solve(data, ...)  
  x$setinversematrix(inversem)
  inversem
}

