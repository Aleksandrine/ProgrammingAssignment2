## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix

## creating a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## running cacheSolve to compute the inverse of the special "matrix"
cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
mat <- matrix(rnorm(9),3,3)
mat_ <- makeCacheMatrix(mat)
cacheSolve(mat_)