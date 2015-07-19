## Create a "matrix" that allows caching and retrieval of its inverse.

## Create a "matrix" with a list that allows setting and getting the matrix, and
## setting and getting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y = matrix()){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Solve for the inverse of the matrix. If cached data exists, returns the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(x, ...)
  x$setinverse(i)
  i
}
