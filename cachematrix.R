## Together, these two functions take a matrix and outputs its inverse.  To improve efficiency, the inverse matrix is cached into memory so that upon look up, if it exists in cache, there will be no need for further computation.  

## The makeCacheMatrix function lays the foundation for the cacheSolve function to retrieve a cached inverse matrix or calculate and cache the inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  setMat <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  getMat <- function() x
  setInv <- function(inv) invMat <<- inv
  getInv <- function() invMat
  list(setMat = setMat, getMat = getMat,
       setInv = setInv, getInv = getInv)
}


## The cacheSolve function checks to see if there is a cached inverse for the given matrix and retrieves it. If not, it proceeds to determine the inverse of a given matrix and caches it.

cacheSolve <- function(x, ...) {
  invMat <- x$getInv()
  if(!is.null(invMat)) {
    message("Getting cached inversed matrix data")
    return(invMat)
  }
  matData <- x$getMat()
  invMat <- solve(matData, ...)
  x$setInv(invMat)
  invMat
}
