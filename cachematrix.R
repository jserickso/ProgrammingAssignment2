## This R source file includes two functions.  One that stores and grabs
## the cached value of the input of a matrix and one that calculates the
## inverse of the matrix.

## This function caches a matrix and its inverse matrix.
## It creates a list of functions that first sets the value of the
## matrix, then gets the value of the matrix, sets the inverse of 
## the matrix, and gets the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y){  ##set the value of the matrix
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x  ##get the value of the matrix
  set_inverse <- function (inverse) inv_mat <<- inverse  ##set the inverse
  get_inverse <- function() inv_mat  ##get the inverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## The function below checks to see if there is a cached value of the
## inverse.  If so, it returns that value. If not, it calculates the
## inverse of the input matrix.

cacheSolve <- function(x, ...) {
  inv_mat <- x$get_inverse()
  if(!is.null(inv_mat)){
    message("Getting cached data...")
    return (inv_mat)  ##Returns the cached inverse matrix
  }
  matrix <- x$get()
  inv_mat <- solve(matrix)
  x$set_inverse(inv_mat)
  inv_mat ## Return a matrix that is the inverse of 'x'
}
