## An inverse cache library. Cache's the inverse of a given matrix
## to save computational cost in future operations.

## Create's the cache object
makeCacheMatrix <- function(x = matrix()) {
  the_inverse <- NULL

  set <- function(y = matrix()) {
    the_matrix <<- y
    the_inverse <<- NULL
  }
  set(x)
  get <- function() the_matrix
  setinv <- function(the_inv) the_inverse <<- the_inv
  getinv <- function() the_inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Returns the cached object, or caches it if not already
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  the_inverse <- x$getinv()  

    if(!is.null(the_inverse)) {
      message("getting cached data")
  }
  else
  {
    the_matrix <- x$get()
    print(class(the_matrix))
    the_inverse <- solve(the_matrix)
    x$setinv(the_inverse)
  }
  the_inverse
}

