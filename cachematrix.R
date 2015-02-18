## Creates a special matrix, which is a list containing a function to
## - set value of the matrix
## - get value of the matrix
## - set value of the inverse matrix
## - get value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) z <<- inv
  getinverse <- function() z
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## Calculate the inverse of the especial matrix created with function
## makeCacheMatrix, getting cached result if it is available 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  y <- x$getinverse()
  if(!is.null(y)) {
    message("getting cached data")
    return(y)
  }
  m <- x$get()
  y <- solve(m, ...)
  x$setinverse(y)
  y
}