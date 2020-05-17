## This function creates a special "matrix" object that is able to cache its inverse##


makeCacheMatrix <- function(x = matrix()) {
     
     makeCacheMatrix <- function(x = matrix()) 
          inv <- NULL
          set <- function(y) {
               x <<- y
               inv <<- NULL
          }
          get <- function() x
          setInverse <- function(inverse) inv <<- inverse
          getInverse <- function() inv
          list(set = set,
               get = get,
               setInverse = setInverse,
               getInverse = getInverse)
     
}

##The following function allows the computation of the inverse of the "matrix" MakeCacheMatrix##
##If the inverse has been calculated it will return the inverse from the cache##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     
     inv <- x$getInverse()
     if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     mat <- x$get()
     inv <- solve(mat, ...)
     x$setInverse(inv)
     inv
}
