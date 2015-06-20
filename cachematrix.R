## Chen Lin
## Uses lexical scoping to calculate inverse of matrix. Assumes input matrix
## is square and has an inverse.

## Creates a list of 4 functions: set, get, setinv, and getinv.  Sets matrix,
## gets matrix using m$get, sets inverse, gets inverse, respectively.

makeMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Returns the inverse of matrix.  If inverse is already calculated,
## function returns stored data.  Otherwise, it calculates with
## solve().

cacheinv <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
