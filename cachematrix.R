# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function( x = numerical()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <-function() x
  
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function( x, ...) {
  
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setInverse(m)
  m
  
}

> m = makeCacheMatrix(rbind(c(2,4), c(4,2)))
> m$get()
     [,1] [,2]
[1,]    2    4
[2,]    4    2
> cacheSolve(m)
           [,1]       [,2]
[1,] -0.1666667  0.3333333
[2,]  0.3333333 -0.1666667
> cacheSolve(m)
getting cached data
           [,1]       [,2]
[1,] -0.1666667  0.3333333
[2,]  0.3333333 -0.1666667
> 
