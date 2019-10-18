## The functions below are designed to compute the inverse of a matrix, checking first whether the inverse has already been calculated

## A function designed to assign the matrix inverse, and retrieve it if it has already been calculated

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## A function designed to take the output of the above function, and return the matrix inverse either by calculation or by retrieval

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}


