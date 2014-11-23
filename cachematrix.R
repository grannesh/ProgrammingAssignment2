## Function for calculation of inverse of any inversible matrix
## After calculation, the inverse will be cached
## It will first lookup to see if the inverse has already been computed, if so use cached value.
## 2014-11-21 Harald Grannes

## Function makeCacheMNatrix
## Create a matrix object with methods set and get inverse 

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Get the inverse of a matrix using the solve function in R
## If the inverse has already been calculated, return the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
