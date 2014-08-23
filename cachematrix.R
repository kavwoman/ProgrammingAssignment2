## Caches the results of calculating the inverse of a matrix
## using lexical scoping.
##
## Functions:
## -- makeCacheMatrix: initialize a special matrix object
## -- cacheSolve: given special matrix object, return inverse

## This function returns a list of functions:
## -- get(): return the matrix
## -- set(y): set matrix to y
## -- getinv(): return the inverse
## -- setinv(inverse): set inverse value to inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function takes as input a makeCacheMatrix output object
## If the inverse of the matrix has already been calculated, it 
## returns that value.  Otherwise, it calculates the inverse
## Assumes matrix is invertible

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
