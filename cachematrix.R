## Richard M. Palmer
## R Programming
## week 3 assignment
##
## Functions to (1) create a special matrix list
## and to (2) compute and cache the inverse of that matrix

## Create a list that consists of functions for storing
## a matrix, as well as calculating and storing its inverse
## Elements of the list:
##  $set        - set the data of the matrix, also clear the inverse
##  $get        - get the data of the matrix
##  $setinverse - set the m
##  $getinverse - retrieve the cached inverse, may be null
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  getinverse <- function() inv
  setinverse <- function(inverse) inv <<- inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Return the inverse of a specially stored matrix
## first check to see if there is a cached inverse
## if so, return it. If not, compute, store and return it.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
