## The two functions created will be used to find the inverse of a matrix x. 
## If the inverse has already been found, the value is returned from the cache instead of recalculating again.


## -----------------------------------
## makeCacheMatrix function creates a special R object that
## 1. Initializes a variable 'm' to save inverse matrix latter as a cached data)
## 2. function get() to obtain the input matrix
## 3. function setinverse() to assign the  inverse matrix (of x) to m;
## 4. Provides function getinverse() to obtain the cached inverse matrix.

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


## This function does  the calc of inverse matrix of x.  It first checks if the in-
## verse matrix has been found,ie., present in the cache; if yes, returns the result and quits. If not, the 
## inverse of x is calculated, saved to cached and returned.


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  else {
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  return(m)

  }
}

