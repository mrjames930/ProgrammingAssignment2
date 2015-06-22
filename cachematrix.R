## The first function will create an inverted matrix, and the second will 

## the makeCacheMatrix creates an empty matrix, and
## then adds the inverted matrices to it. It will:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invMatr <- NULL
  setVal <- function(y) {
    x <<- y
    invMatr <<- NULL
  }
  getVal <- function() x
  setinverse <- function(inverse) invMatr <<- inverse
  getinverse <- function() invMatr
  list(setVal = setVal, 
       getVal = getVal,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes the result of makeCacheMatrix and returns a matrix
## that is the inverse of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mInv <- x$getinverse()
  if(!is.null(mInv)) {
    message("getting cached data")
    return(mInv)
  }
  data <- x$getVal()
  m <- solve(data, ...)
  x$setinverse(mInv)
  mInv
}

