## The following functions enable us to calculate the matrix inverse
## and to cache it. This helps us checking if an inverse for a specific matrix has already
## been calculated. If so, no new calculation needs to be conducted, saving computing resources
## and time.

## makeCacheMatrix returns a list of functions to set and get a matrix, and to
## set and get the inverse of that particular matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes the output from makeCacheMatrix to first check whether an inverse
## for the particular R Object has been calculated. If so so, the inverse is returned.
## Otherwise the inverse is calculated, cached and then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}