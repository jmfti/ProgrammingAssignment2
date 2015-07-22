## Put comments here that give an overall description of what your
## functions do

## this functions accepts a matrix as argument and stores a variable 'mymat' which will be used to store
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  mymat <<- x
  cachem <<- NULL
  
  getInverse <- function() return(cachem)
  setInverse <- function(x) cachem <<- x
  
  getMatrix <- function() return(mymat)
  setMatrix <- function(x) mymat <<- x
  
  list(getMatrix=getMatrix, setMatrix=setMatrix, getInverse=getInverse, setInverse=setInverse)
}


## cacheSolve accepts a previous created matrix with makeCacheMatrix
## and it looks for the inverse matrix into the variable mymat.
## if the variable is null then the inverse matrix hasn't been computed so it
## will compute it and it will store in mymat

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if (is.null(x$getInverse())){
    message("matrix not in cache, getting it")
    x$setInverse(solve(x$getMatrix()), ...)
  }
  x$getInverse()
}


m <- makeCacheMatrix(matrix(rnorm(9,1), 3,3))
solved <- cacheSolve(m)
solved

solved <- cacheSolve(m)
