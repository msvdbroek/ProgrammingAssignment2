## sets the matrix, make the inverse NULL
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix
## give all functionality as a list back as a result

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## checks if the inverse result already exists:
## if so, use cached result
## if not, inverse the matrix

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data = x$get()
  inv = solve(data, ...)
  x$setinv(inv)
  inv
}
