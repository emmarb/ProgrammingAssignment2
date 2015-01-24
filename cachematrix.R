# These functions calculate the inverse of a matrix and save the result in a cache to avoid calculate it again.

# makeCacheMatrix returns a list of functions to set/get a matrix and set/get its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        # m the inverse matrix
        m <- NULL
        
        # set function to change the cached value
        set <- function(y) {
                # x is the given matrix
                x <<- y
                m <<- NULL
        }
        
        # get the cached value for the matrix
        get <- function() x
        
        # given a matrix, set its inverse matrix value
        setinverse <- function(solve) m <<- solve
        
        # given a matrix, get its inverse matrix value
        getinverse <- function() m
        
        # returns a list of functions related to the given matrix and its inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# cacheSolve caclulates the inverse of a matrix or gets its value from a cache
cacheSolve <- function(x, ...) {

        # Return a matrix that is the inverse of 'x' calling getinverse function
        m <- x$getinverse()
        
        # if the inverse of the given matrix exists in the cache, then returns it
        if(!is.null(m)) {
                # print a message for console
                message("getting cached data")
                return(m)
        }
        
        # if the cache does not contain the inverse value for the given matrix, then calculates it
        # get the value for the given matrix
        data <- x$get()
        # calculates the inverse of the given matrix
        m <- solve(data)
        # set the value of the inverse matrix in the cache
        x$setinverse(m)
        
        # returns the inverse matrix
        m
}
