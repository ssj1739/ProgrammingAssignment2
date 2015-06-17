## This function caches the inverse of a matrix 
## using the solve function in R.

## This function constructs the cache as a list
## of four functions, allowing the user to set
## or get the matrix, as well as set or get the
## inverse of the matrix.  

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inverse) {
        i <<- inverse
    }
    
    getinverse <- function() {
        i
    }
    
    return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}


## This function allows for the inverse
## of the matrix to be calculated using
## the solve function.  However, before
## doing so, it checks the cache to see
## if the inverse has already been 
## calculated.  If it has, the function
## returns the already calculated
## inverse.  If not, it calculates the
## inverse itself using solve().

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data...")
        return(i)
    }
    matrix <- x$get()
    i <- solve(matrix, ...)
    x$setinverse(i)
    return(i) 
}
