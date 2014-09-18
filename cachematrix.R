## The R Script file cachematrix.R contains two functions that cache the inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse_matrix <- function(inverse_matrix) m <<- inverse_matrix
  get_inverse_matrix <- function() m
  list(set = set, get = get,
       set_inverse_matrix = set_inverse_matrix,
       get_inverse_matrix = get_inverse_matrix)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## It retrieves the inverse from the cache if the inverse has already been calculated and the matrix has not changed.

cacheSolve <- function(x, ...) {
  m <- x$get_inverse_matrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse_matrix(m)
  m
}
