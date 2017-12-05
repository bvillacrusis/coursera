## The following functions store the matrix and its computed inverse in the cache.
## If the inverse of the matrix will be called again, the function
## will look up in the cache and returns it. If it is not
## in the cache, the function cacheSolve will solve it again and stores it.


## this function creates a list that stores the 
## input matrix and its inverse
  makeCacheMatrix <- function(x = matrix()) {
      inverseMatrix <- NULL
      ## setters of matrix and its inverse, resp.
      set <- function(y) {
          x <<- y
          inverseMatrix <<- NULL
      }
      setInverse <- function(inverse) inverseMatrix <<- inverse
      
      ## getters of matrix and its inverse, resp.
      get <- function() x
      getInverse <- function() inverseMatrix
      
      ## returns this list
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
  }


## This function returns the inverse of a matrix.
## It retrieves the inverse from the cache if it was previously computed.
## If not, it recomputes the inverse of the matrix and store it again in the cache.
##It assumes that the matrix is invertible, otherwise, an error message will be thrown.

  cacheSolve <- function(x, ...) {
      m <- x$getInverse()
      if(!is.null(m)) {
          message("Getting cached data.")
          return(m)
      }
      data <- x$get()
      
      # computing the inverse
      m <- solve(data, ...)
      
      # caching the computed inverse
      x$setInverse(m)
      
      # retuns the inverse
      m
  }
