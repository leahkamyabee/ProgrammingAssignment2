## There are two functions defined in this file

## First is a makeCacheMatrix, assuming a square inversible matrix input
## this function caches the inverse of its input

## Second is a cacheSolve function, that actually computes the inverse of the matrix

## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x #get the value of the matrix
  setmatrix <- function(solve) m <<- solve #set the value of the inverse matrix
  getmatrix <- function() m #get the value of the inverse matrix
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## cacheSolve
## This function computes the inverse of the special "matrix" returned by 
## the makeCacheMatrix function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) { #check to see if the inverse has been cached)
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
