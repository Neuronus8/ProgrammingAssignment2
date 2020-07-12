## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This is a function, that automatically check input matrix
## for being square matrix. After that, if conditions fulfilled, it caches
## solved matrix. Otherwise, it cuts matrix to a square one, losing
## some data in process

makeCacheMatrix <- function(x = matrix()) {
  is.whole <- function(a) { 
    (is.numeric(a) && floor(a)==a) ||
      (is.complex(a) && floor(Re(a)) == Re(a) && floor(Im(a)) == Im(a))
  }
  checks <- sqrt(length(x))
  if(is.whole(checks)) {
    cols <- sqrt(length(x))
    rows <- cols
    x <- matrix(x, nrow = rows, ncol = cols)
    s <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    lst <- list(set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve)
  } else {
    cols <- ceiling(sqrt(length(x)))
    rows <- cols
    x <- matrix(x, nrow = rows, ncol = cols)
    s <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    lst <- list(set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve)
  }
}


## Write a short comment describing this function
##Not gonna lie, I took example as a prototype for this function
##As it was, It checks data for being empty. If not, it returns a solved matrix
##Otherwise, it gets a matrix and solve it here to return.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
