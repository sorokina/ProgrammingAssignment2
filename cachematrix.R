## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # cached inverse of matrix
  inver <- NULL
  
  ## getter/setter for matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  
  ## get and set inverse matrix
  getinv <- function() inver
  setinv <- function(inverse) inver <<- inverse
  
  ## return list of functions for matrix
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inver <- x$getinv()
  
  # return cached matrix inverse if it's been already computed
  if (!is.null(inver)) {
    message("inverse is cached")
    return(inver)
  }
  
  # compute inverse of matrix 
  m <- x$get()
  inv <- solve(m, ...)
  
  # cache
  x$setinv(inver)
  
  # return inverse 
  return(inver)        ## Return a matrix that is the inverse of 'x'



}
