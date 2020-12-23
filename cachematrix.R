## These pair of functions cache the inverse of a matrix!


## makeCacheMatrix creates a special "matrix" object that 
## can cache its inverse
makeCacheMatrixfx <-function(x=matrix()){
  inv <-NULL             
  set <- function (y){          # Set matrix value
    x <<- y                     # cache the matrix
    inv <<-NULL
  }
  
  get <- function() {x}
  setInverse <- function (inverse) {inv <<- inverse}
  getInverse <- function () {inv}
  list (set = set, get = get, 
        setInverse = setInverse, 
        getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above. If 
## the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <-function(x, ...){
  inv <- x$getInverse()                   # if an inverse has already been calculated this gets it
  if (!is.null(inv)) {                    # check to see if cacheSolve has been run before
          if(x$set() == x$get()) {        # check that matrix hasn't changed, and if it hasn't, sends a text message and returns the cached matrix
    message("getting cached data")
    mat <- x$get()
    inv <- solve(mat, ...)        
    x$setInverse(inv)
    return(inv)
   }
  #otherwise
  y <- x$get()
  x$set(y)
  inv <- solve(y, ...)
  x$setInverse(inv)
  inv
}
      
