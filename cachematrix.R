## these functions creates matrix capable of storing the inverse matrix into a variable
## so it can be retrieved again without the need of computing it again.

## this functions accepts a matrix as argument and stores a variable 'mymat' which will be used to store
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  mymat <<- x
  cachem <<- NULL
  
  getInverse <- function() return(cachem) # retrieve inverse matrix from cache
  setInverse <- function(x) cachem <<- x  # store inverse matrix into cache
  
  getMatrix <- function() return(mymat)  # retrieve original matrix
  setMatrix <- function(x) mymat <<- x   # store our matrix
  
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
    solvedmatrix <- solve(x$getMatrix())  # we get the solved inverse matrix
    x$setInverse(solvedmatrix, ...) # store the inverse matrix into the cache
  }
  x$getInverse()  # once generated (or if it already exist) we return the inverse matrix
}


m <- makeCacheMatrix(matrix(rnorm(9,1), 3,3))
solved <- cacheSolve(m)
solved
