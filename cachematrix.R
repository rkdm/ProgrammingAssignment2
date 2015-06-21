## Programming Assingment 2
##  includes function "makeCacheMatrix" which creates a matrix 
##  object and can calculate and cache the inverse of that matrix

## This contains functions to:
##  set the matrix
##  get the matrix
##  set the inverse of that matrix
##  get the inverse of that matrix
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


## This function checks to see if the inverse has already
##  been calculated; if so, the inverse is returned; if not, 
##  the inverse is calculated and then returned.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
    
    ## Return a matrix that is the inverse of 'x'
}
