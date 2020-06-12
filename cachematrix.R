# The two functions makeCacheMatrix and cacheSolve have the objective respectively of:
# - creating a special matrix that can cache it inverse
# - computing the inverse of the special matrix or retrieving it from the cache in case it has already been computed,
#   thus saving precious computation time


# The following function takes a numeric matrix as input and creates a "special" matrix object (actually a list) that can cache its inverse.
# It contains four functions that can set and get the matrix and its inverse respectively
# Since the ultimate objective is the computation of the inverse, the matrix x in input needs to be invertible, or cacheSolve will throw an error
makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# The following function takes the special matrix created through "makeCacheMatrix" as input and either computes its inverse
# and stores it in the cache or retrieves it from the cache if it has already been computed.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Retrieving cached data")
    return(inv)
  } else {
    message("Computing inverse")
    inv <- solve(x$get(), ...)
    x$setInverse(inv)
    return(inv)
  }
}
  

# Code for quality assurance check
# First check the message should be "Computing inverse"; second check "Retrieving cached data"
mat <- matrix(c(2,0,0,2), nrow = 2)
specialMat <- makeCacheMatrix(mat)

check1 <- cacheSolve(specialMat)

check2 <- cacheSolve(specialMat)

