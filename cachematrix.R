##  Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly.

##There are 2 functions associated with this file.  The first makeCasheMatrix instantiates an object that will set and get,
## a matrix and set and get the inverse of the function.

## The second function cacheSolve solves the stored matrix and sets the Inverse.  If the inverse has already been calculated,
## it will abort the solve and retrieve the cached matrix.

## Steps to Use these functions
##  1.  Create an invertable square matrix A <- matrix(c(4,7,2,6),nrow=2, ncol=2, byrow = TRUE)
##  2.  Create a list of functions with the makeCasheMatrix function.  test<-makeCasheMatrix()
##  3.  Assign the matrix using the test$set function.  test$set(A)
##  4.  Run cacheSolve function cacheSolve(test)
##  5.  Run CacheSolve agains and not that cached matrix is used
##  6.  Retrieve cached inverse matrix using test$getInverse()
##  7.  Test inverse by multiplying the matrix by its inverse to get the identity matrix.  test$get()%*%test$getInverse()
  

makeCasheMatrix <- function(x = numeric()) {
  invMat <- NULL
  set <- function(y) {  ## set function
    x <<- y
    invMat <<- NULL
  }
  get <- function() x ## get function
  setInverse <- function(inverse) invMat <<- inverse  ## setInverse function
  getInverse <- function() invMat  ## getInverse function
  list(set = set, get = get,  ## Returns function list
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  invMat <- x$getInverse()
  if(!is.null(invMat)) { ## test for presence of cached data
    message("getting cached data")
    return(invMat)
  }
  data <- x$get()  ## Retrieve the stored matrix, calcluate inverse and store the result.
  invMat <- solve(data, ...)
  x$setInverse(invMat)
  invMat
}
