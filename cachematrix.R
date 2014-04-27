# Author
# Costanzo Di Maria
# 27/04/2014


## makeCacheMatrix caches a matrix an its inverse.
## cacheSolve returns the inverse of a matrix.



## makeCacheMatrix provides methods to set and retrieve 
## from cache a matrix and its inverse.

makeCacheMatrix <- function(m = matrix()) {
  
        minv <- NULL        # Initialise inverse matrix
        
        # Method to set value of matrix m
        set <- function(y) {
                m <<- y
                minv <<- NULL
        }
        
        # Method to retrieve matrix m
        get <- function() m
        
        # Method to set value of inverse of m, minv
        setInverse <- function(inv) minv <<- inv
        
        # Method to retrieve the value of minv
        getInverse <- function() minv
        
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}



## cacheSolve returns the inverse of a matrix m.
## If the inverse is already in cache than this matrix is retrieved
## from cache.
## If the inverse in not in cache, the value is calculated and cached
## so that it will not need to be re-calculated in future.

cacheSolve <- function(m, ...) {
        # Retrieve current value of cached inverse. 
        # NULL if not calculated before.
        minv <- m$getInverse()
        
        if (!is.null(minv)) {                              # If the inverse already exists in cache
                message("Retrieving data from cache...")
                return(minv)                               # just return value from cache
        }
        
        # If value was not in cache
        message("Calculating inverse matrix...")
        data <- m$get()
        minv <- solve(data)       # calculate inverse
        m$setInverse(minv)        # and cache inverse
        
        minv                      # Return inverse matrix.
}
