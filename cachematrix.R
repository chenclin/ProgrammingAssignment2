## Chen Lin
## Uses lexical scoping to calculate inverse of matrix. Assumes input matrix
## is square and has an inverse.


makeMatrix <- function(x = matrix()) {
## Creates a list of 4 functions: set, get, setinv, and getinv.  Sets matrix,
## gets matrix using m$get, sets inverse, gets inverse, respectively.
  
  m <- NULL
## set up matrix (lexical scoping)
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
## get matrix
  get <- function() x

## set inverse
  setinv <- function(inv) m <<- inv
## get inverse
  getinv <- function() m

## list of 4 functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
## Returns the inverse of matrix.  If inverse is already calculated,
## function returns stored data.  Otherwise, it calculates with
## solve().
  
  m <- x$getinv()
## if inverse already calculated
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  data <- x$get()
## calculate inverse
  m <- solve(data, ...)
  x$setinv(m)
  m
}
