## The following functions' goal is to save some computation 
## cost in calculating the inverse of a matrix (useful in Linear Algebra)
## by caching it instead of computing it every time.

## In order to cache it we'll create two functions:
## makeCacheMatrix - which creates a special matrix object 
## that can cache its inverse
## cacheSolve - which either computes or retrieves the inverse
## from the cache of that makeCacheMatrix object


## makeCacheMatrix takes a matrix as an argument
## and provides the functions and objects to allow
## storing the inverse matrix calculated by the cacheSolve() function
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

## cacheSolve takes a makeCacheMatrix object as an argument
## and checks if that object has an inverse matrix in cache
## and either returns that or calculates the inverse
## and caches it and returns it
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
