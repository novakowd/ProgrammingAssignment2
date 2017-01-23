## First function defines set and get functions for a matrix "x" 
## and it's inverse "i"

## Second function calls on the first function and calculates the 
## inverse of matrix "x"

## Function which outputs a list containing 4 functions 
## and includes two data objects "x" and "i"

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Calculates the inverse of the matrix above and caches value 

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}


