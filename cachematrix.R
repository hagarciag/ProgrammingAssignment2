## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  minverse <- NULL
  set <- function(y) {
    x <<- y
  }
  get <- function() x
  getm <- function() m
  setinversematrix <- function(inversematrix) {m <<- x; minverse <<- inversematrix}
  getinversematrix <- function() m
  list(set = set, get = get, getm = getm,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
  }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getm()
  inversem <- x$getinversematrix()
  currentm <- x$get()
  if(!is.null(m) && identical(m, currentm)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversematrix(m)
  m
}

