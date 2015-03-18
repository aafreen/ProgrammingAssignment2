## A function pair that cache the inverse of a matrix.

##makeCacheMatrix: creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ##set the value of the vector
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ##get the value of the vector
  get <- function() x
  
  ##set the value of the inverse
  setinverse <- function(inverse) i<<- solve
  
  ##get the value of the inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


##cacheSolve: computes the inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse
  i
}
