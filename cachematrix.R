## makeCacheMatrix will cache the inverse of matrix
## cacheSolve will use the makeCacheMatrix to optimize inverse calculations

## Calculate inverse and cache it.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(invers) inv<<-invers
  getinverse<-function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Store the inverse of matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse value")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat)
  x$setinverse(inv)
  inv
}
