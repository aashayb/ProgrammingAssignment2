## The functions enable caching the inverse of a matrix

## The function "makeCacheMatrix" creates a special matrix, 
## which in actaulity is a list, used to enable caching the inverse of a matrix.
## 4 functions are defined in the function given below.
## set the value of the vector
## get the value of the vector
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<-NULL
  }
  get <- function() x
  setinv <- function(y) inv <<- y
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function returns the inverse of an invertible matrix
## after checking its cache reserve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    return(inv)
  }
  orig <- x$get()
  print(orig)
  inv <- solve(orig,...)
  x$setinv(inv)
  inv
}
