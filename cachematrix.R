## Naomi Greene
## February 7, 2016
## Programming Assignment 2


## This set of functions create a cache version of a matrix
## and then either return the inverse a matrix input by the user
## or return the cache version of the matrix

## This function takes a matrix as an argument and 
## returns a list of functions for the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)         
                

}


## This function takes a list produced by the function "makeCacheMatrix."
## First, the function checks to see if the list provided a saved version 
## of the inverse of the matrix and returns it. Otherwise it calculates
## the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
        message("getting cached data")
        return(m)
        }
  
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
