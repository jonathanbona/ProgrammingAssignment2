## These functions take advantage of R's scoping rules to "cache" potentiall expensive 
## matrix inverse operations



## makeCacheMatrix creates a special "matrix", which is a list of functions to:
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix's inverse
## get the value of the matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  # returns a list of these functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates the inverse of a "matrix" created by makeCacheMatrix
## If the inverse has already been calculated and cached, return it. 
## Otherwise: calcaulate, cache, and return it.
cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  # already cached? return it
  if(!is.null(i)){
    message("getting cached inverse")
    return(i)
  }

  data <- x$get() # get the matrix
  i <- solve(data) # calculate its inverse
  x$setinverse(i) # cache the inverse
  i # return it
}
