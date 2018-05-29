## These functions are constructed to essentially give an inverse of a matrix
##The matrix could be a new one or an already stored one

## This function creates a special "matrix", which prepares the function to get the inverse of the resulting matrix

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function(y) {
    x<<- y
    m<<- NULL
  }
  get <-function() x
  setinverse<- function(inverse) m<<-inverse
  getinverse<- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function returns the inverse of the resulting matrix given by makeCacheMatrix function above
## The function will retrieve the inverse from the cache if the function has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<- x$get()
  m<- solve(data, ...)
  x$setinverse(m)
  m
}
