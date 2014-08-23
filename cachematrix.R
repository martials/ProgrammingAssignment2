## Programming Assignment 2: Lexical Scoping
## Computation and starage of matrix inverse


## creates a cache matrix function, this is a function object to store and retrive a matrix 
## and its inverse i.e. inverse of an matrix. Here we have the data.

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setinverse <- function(x) inv <<- x
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## provides the inverse of a matrix by eighter computing it or returning a cached value from 
## a makeCacheMatrix function object. Here we have the functionality

cacheinverse <- function(cm, ...) {
  inv <- cm$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- cm$get()
  inv <- solve(data, ...)
  cm$setinverse(inv)
  inv
}
