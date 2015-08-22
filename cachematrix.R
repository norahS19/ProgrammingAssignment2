## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns the inverse of the matrix object returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  if(!is.null(m)){
    message("getting cached data")
    return (m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
