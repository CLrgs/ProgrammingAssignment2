

## "makeCacheMatrix" is a function, that takes as argument a matrix "x" and :
## 1. Returns in the parent environment a list of 4 named functions :
##      1.1 set(y) 
##      1.2 get()
##      1.3 setinverse(inverse)
##      1.4 getinverse()
## 2. Two R objects in the parent environment :
##      2.1 x (matrix)
##      2.2 inv 
        


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }                
        setinverse <- function(inverse) {
                inv <<- inverse
        }
        getinverse <- function() {
                inv
        }        
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## "cacheSolve" returns a matrix that is inverse of matrix(x),in the following way:
## checks if the current state of inverse is not NULL ( = inversion has already been computed)
##      if YES returns the value of "inv" from the parent environment
##      if NO ( = not been computed) , makes the inversion of the matrix ("solve"), stores and returns the result.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Getting cached matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv 
}
