## Caching the Inverse of a Matrix


## Create a matrix with an inverse that can be stored in cache

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
        x <<- y
        m <<- NULL
        }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  }


## Solve the inverse of a matrix or if its already solved and stored in cache retrieve that inverse

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
              message("getting cached data")
              return(m)
              }
  XX <- x$get()
  m <- solve(XX, ...)
  x$setInverse(m)
  m
}
