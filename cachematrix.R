## The pair of functions below cache the inverse of a matrix.

## Creates a special "matrix" object, which is in fact a list of functions to
## set the value of the matrix, get the value of the matrix, set the value of
## the inverse of the matrix, and get the value of the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse <<- inverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of the special "matrix" created with the above function:
## If the inverse has already been calculated, it gets the inverse from the
## cache; otherwise, it calculates the inverse of the "matrix" and sets the
## value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
