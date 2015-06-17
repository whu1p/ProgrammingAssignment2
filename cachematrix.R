## This file contains functions designed to obviate the need to recalculate the
##     inverse of a matrix which was calculated previously.  These functions
##     calculate the inverse of a matrix if it has not been previously
##     calculated and then cache the result.  If the inverse had been
##     previously cached, the cached result is retrieved.


## This function will create a special matrix object with methods which can be invoked
##     to set and get the value of a matrix, and to set and get the value
##     of the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y) {
        x <<- y
		    i <<- NULL
        }
      
        get <- function() x
  
        setinverse <- function(solve) i <<- solve
        
        getinverse <- function() i
        
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}

## This function will take a special matrix object x as created by the function
##     makeCacheMatrix(), and will return the inverse of x.

cacheSolve <- function(x, ...) {
	      ## If the inverse of x has been previously cached, print a message & return the cached
      	## inverse of x.
	      i <- x$getinverse()
	      if(!is.null(i)) {
		            message("getting cached data")
		            return(i)
	      }

	      ##  Otherwise, calculate the inverse of x, cache it, and return it.)
	      data <- x$get()
      	i <- solve(data, ...)
	      x$setinverse(i)
	      i
}
