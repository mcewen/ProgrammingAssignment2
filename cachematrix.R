## makeCacheMatrix() is a function that generates a list of four
## funcitons:
## (1) set = set, used by "x$set()" sets the content of the matrix.
## (2) get = get, used by "x$get()" gets or returns the content of 
## the matrix.
## (3) setinverse = setinverse, used by "x$setinverse()" sets the
## content of the inverted matrix.
## (4) getinverse = getinverse, used by "x$getinverse()" gets or
## returns the content of the inverted matrix.
##
## "a <- makeCacheMatrix()" will give a list of four functions that
## can be called with the $: (see above).

makeCacheMatrix <- function(x = numeric(matrix())) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(mtrx) m <<- mtrx
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve() is another function that looks in the element, 
## passed in as an argument, and will return a matrix that
## is the inverse of 'x', or it will calculate it and store it via
## the appropriate function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
