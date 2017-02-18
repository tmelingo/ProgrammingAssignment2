
### This function sets the value of the matrix, gets its value,
### sets the value of the inverse matrix and
### gets its value
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solveMat) inv <<- solveMat
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
### Calculating the inverse of the matrix created with the first function
### then using cached result if available, including a message when doing so
    
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  dat <- x$get()
  inv <- solve(dat)
  x$setInverse(inv)
  inv
}
