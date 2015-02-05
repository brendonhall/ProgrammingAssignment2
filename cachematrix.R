## This program creates a wrapper for a matrix objecg
## It lets the matrix cache its own inverse, which gets flushed if 
## the matrix changed in any way

## create a new matrix object that can hold on to it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<-NULL
  }
  
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set=set, get=get, getinverse=getinverse, setinverse=setinverse)
}


## find the matrix inverse, but used cached value if it is available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()

  #check if matrix has cached inverse
  if (!is.null(inverse)) { #if it does, return it
    message("getting cached data")
    return(inverse)
  }
  
  #otherwise, need to compute the new matrix inverse
  data <- x$get()
  inverse <- solve(data,...)
  #cache the newly computed inverse
  x$setinverse(inverse)
  inverse
  
}
