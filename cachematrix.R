## Coursera Assignment

## Function for caching


makeCachematrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y=matrix()) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function for computing inverse of square matrix

cachesolve <- function(x, ...) {
  i <- x$getinverse
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
        ## Return a matrix that is the inverse of 'x'
