## The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
makeCacheMatrix <- function(x = matrix()) {

inv <- NULL

    # 1. set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # 2. get the value of the matrix
    get <- function() x
    # 3. set the value of inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    # 4. get the value of inverse of the matrix
    getinverse <- function() inv
    
    #Return a list of methods
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    
    # Returns the inverse matrix of 'x'
    inv <- x$getinverse()
    
    # Just return the inverse if its already set
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    # Get the matrix from our object
    data <- x$get()
    inv <- solve(data)
    # Set the inverse to the object
    x$setinverse(inv)
    # Return the matrix
    inv
}







