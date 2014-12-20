## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: "This function creates a special "matrix" object that can cache its inverse."

makeCacheMatrix <- function(x = matrix()) {
    # member variables
    inverse <- NULL
    
    # member functions
    # set: replace our object with an entirely new matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    # get: fetches just the matrix
    get <- function() x
    
    # set/get inverse: set/fetch the member variable 'inverse'
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    
    # returns a list of member functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: "This function computes the inverse of the special "matrix" returned by makeCacheMatrix above."

cacheSolve <- function(x, ...) {
    # first try to get inverse from the cache
    i <- x$getinverse()
    if(!is.null(i)) {
        # found in cache, return it
        message("getting cached data")
        return(i)
    }
    # not in cache: first compute it from the raw matrix data
    data <- x$get()
    i <- solve(data, ...) # (this will fail if the matrix is not invertible)
    # now store it in the cache
    x$setinverse(i)
    # return our newly computed inverse
    i
}
