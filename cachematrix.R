
## Factory for matrices that can cache their inverse value.
## *originMatrix* is assumed to be an invertible square matrix.
makeCacheMatrix <- function(originMatrix = matrix()) {
  cachedInverse <- NULL
  
  set <- function(newMatrix) {
    originMatrix <<- newMatrix
    cachedInverse <<- NULL
  }
  
  get <- function() {
    originMatrix
  }
  
  setInverse <- function(inverse) {
    cachedInverse <<- inverse
  }
  
  getInverse <- function() {
    cachedInverse
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns the inverse of *matrixWithCache* (built with the *makeCacheMatrix* factory).
## Extra arguments similar to the *solve* operation.
cacheSolve <- function(matrixWithCache, ...) {
  cachedInverse <- matrixWithCache$getInverse()
  if(!is.null(cachedInverse)) {
    return (cachedInverse)
  } else {
    data <- matrixWithCache$get()
    computedInverse <- solve(data, ...)
    matrixWithCache$setInverse(computedInverse)
    computedInverse
  }
}
