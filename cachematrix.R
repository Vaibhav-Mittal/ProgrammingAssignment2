## Put comments here that give an overall description of what your
## functions do
## These functions give the inverse of a matrix
## if it already exists, it gives it from the cache
## otherwise it produces the inverse and gives it

## Write a short comment describing this function
## This function returns a list of function variables set, get, setinverse and
## getinverse. 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function returns the inverse of the matrix. if it exists in the cache, it
## gives it from there, otherwise it calculates it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
