## Put comments here that give an overall description of what your
## functions do

## Function creates special matrix, containing function to set matrix, get matrix, get inverse matrix, set inverse matrix  

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function()x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get=get, 
       getinverse = getinverse,
       setinverse = setinverse)
}


## Calculates inverse of the special matrix. First checks to see if inverse has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)){
      message("getting cached data")
      return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
