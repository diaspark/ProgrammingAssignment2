## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Vinod Jain.  May 15, 2016
## makeCacheMatrix.  The function is similar to makeVector
## it makes a special matrix with the fn: set, get, setmatr, getmatr
## it caches matrix (that's x) and its inverse (that's m)


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatr <- function(matr) m <<- matr
  getmatr <- function() m
  list(set = set, get = get,
       setmatr = setmatr,
       getmatr = getmatr)
  
  ## this is comment line
}


## vinod jain 

## Write a short comment describing this function
## cacheSolve is similar to cachemean
## The following function calculates the inverse  of the special "matrix"
## created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and
## sets the value of the inverse in the cache via the setmatr function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatr()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatr(m)
  m
}
