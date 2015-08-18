## Two functions are created. The first, "makeCacheMatrix", defines a special
## matrix object with allied get/set and getMatrixInverse/setMatrixInverse methods.
## The second function, "cacheSolve", takes and instance of the "makeCacheMatrix"
## hyper-matrix object and extracts the inverse of the object from cache (if it exists)
## or calculates it (if it doesn't reside in cache).

## The function "makeCacheMatrix"

## The first function, makeVector creates a special "matrix".
## This is really a list containing a function to
## o set the value of the matrix "A"
## o get the value of the matrux "A"
## o set the value of its inverse "M"
## o the value of this inverse "M"

makeCacheMatrix <- function(X = matrix()) {
  M <- NULL
  set <- function(Y) {
    X <<- Y
    M <<- NULL
  }
  get <- function() X
  setMatrixInverse <- function(matrixInverse) M <<- matrixInverse
  getMatrixInverse <- function() M
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## The function "cacheSolve"

## The following function calculates the inverse "M" of the special "matrix", "X" say,
## created with the above function: "makeCacheMatrix". 
## However, it first checks to see if the matrix inverse "M" has been precomputed.
## If so, it gets the matrix inverse "M" from the cache and skips the inversion. 
## Otherwise, it inverts the matrix "X" and sets the resultant matrix "M" in the cache
## via the setMatrixInverse function.

cacheSolve <- function(X, ...) {
  ## Return a matrix "M" that is the inverse of the matrix "X"
  M <- X$getMatrixInverse()
  if(!is.null(M)) {
    message("getting cached matrix inverse")
    return(M)
  }
  A <- X$get()
  M <- solve(A)
  X$setMatrixInverse(M)
  M
}
