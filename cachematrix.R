## Put comments here that give an overall description of what your
## functions do
## This file contains two functions, makeCacheMatrix() and cacheSolve(), which 
## work together to calculate and cache the inverse of a matrix. 
## Each of these functions is described in more detail below.

## Write a short comment describing this function
## makeCacheMatrix creates a "special" matrix object that can cache its inverse.
## This function takes a matrix as its only argument (and will default to a 1x1 empty matrix)
## It returns a list that contains four functions: set(), get(), set_inverse(),
## and get_inverse(). The resulting "special" matrix object will have acess to
## the environment of makeCacheMatrix() in order to access and manipulate the starting
## matrix and the cached or calculated inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list( set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function
## cacheSolve() is used in conjunction with makeCacheMatrix(). cacheSolve() takes
## on primary argument, x, which is an object of the type makeCacheMatrix(). 
## It will return the inverse of the matrix that was originally passed to
## makeCacheMatrix(). If this is the first time it has done this calculation,
## then the result will be cached in the memory space occupied by the 
## first call of makeCacheMatrix(). For subsequent calls to cacheSolve(), the
## function will retrieve the previously calculated result and return that in lieu
## of rerunning the inversion. In either case, the return of cacheSolve() will be
## the same.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if(!is.null(inv)){
    message("Getting cached data...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  inv
}
