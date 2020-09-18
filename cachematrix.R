## Coursera R Programming Week 3 Homework

## The makeCacheMatrix will be an object that takes a matrix and has a placeholder for storing its inverse.

## The cacheSolve will expect to receive the object of makeCacheMatrix and if the 
## the cache is found return it, otherwise compute and store it.

## This function will take a matrix and store a copy and have a means to get a cache and store it

makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  set <- function(y){
    x <<- y
    c <<- NULL
  }
  get <- function() x
  
  setInverse <- function(invMatrix) c <<- invMatrix
  getInverse <- function() c
  
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function will handle for the inverse of an object, expect to pass a matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  pc <- x$getInverse()
  
  if(!is.null(pc)){
    message("getting cached data")
    return (pc)
  }
  
  mainMatrix <-x$get()
  pc <- solve(mainMatrix, ...)
  x$setInverse(pc)
  pc
}
