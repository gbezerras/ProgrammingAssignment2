## Matrix inversion is usually a costly computation. The functions defined in
## this file allow caching the inverse of a matrix, so that we don't need to
## calculate the inverse every time we need it.


## This function creates a special "matrix" object that can cache its inverse.
##
## It is really just a list containing functions to set/get the matrix and its
## inverse. "setInverse" does not make any calculations.

makeCacheMatrix <- function(x = matrix()) {
        # We store the inverse matrix internally
        inv <- NULL
      
        # Our set() function receives a matrix as parameter
        set <- function(y) {
                x <<- y
                # Invalidate the inverse matrix whenever our matrix changes
                inv <<- NULL
        }
        
        # get(), setinverse() and getinverse() are usual getters and setters
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
      
        # Return a list with the 4 "public" functions
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" created with
## makeCacheMatrix(). 
##
## If the inverse has already been calculated (and the matrix has not changed),
## it gets the inverse from the cache and skips the computation. Otherwise, it
## calculates the inverse of the matrix and sets it in the cache via the 
## setinverse function.

cacheSolve <- function(x, ...) {
        # Try to get the inverse matrix from cache
        inv <- x$getinverse()
        
        # If the inverse is already cached, just return it without calculating
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
      
        # Calculate the inverse matrix and store it in cache  
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        
        # Return the inverse matrix
        inv
}
