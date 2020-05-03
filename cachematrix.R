## The functions below will be caching the inverse of a matrix instead of computing it repeatedly.

## This function will create a special "matrix" object that will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <-function(y){
    x <<-y
    inv <<- NULL  
  }
  get<-function()x
  setInvmatrix < - function(inverse) inv <<- inverse
  getInvmatrix < - function() inv
  list(set = set, get = get,
       setInvmatrix = setInvmatrix,
       getInvmatrix= getInvmatrix)
}


## This function will compute the inverse of the special "matrix" returned by the makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then it will cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInvmatrix()
  if(!is.null(inv)) {
    message("getting cached inverted matrix data")
    return(inv)
  }
  input<- x$get()
  inv <- solve(input,...)
  x$setInvmatrix(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
