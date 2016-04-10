## The functions below are going to cache the inverse of matrix, so it can be printed without being calculated multiple times.


## This function creates a special matrix, which is used to cache data. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function takes the previous one (makeCacheMatrix()) as an argument, after which it calculates and prints its inverse.
## However, it first checks whether the inverse was already calculated. 
## If so, it gets the inverse from the cache and skips computation. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- (solve(data))
  x$setinv(inv)
  inv
}


