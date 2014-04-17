## This pair of functions (makeCacheMatrix and cacheSolve) allows the inverse of
## a matrix to be cached and later retrieved rather than compute it each time it
## is required.


## makeCacheMatrix converts an input matrix into a list of functions to enable 
## caching of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL        
        set <- function(y) { ## set matrix value
                x <<- y
                inv <<- NULL
        }
        get <- function() x  ## get matrix value     
        setinv <- function(calcinv) inv <<- calcinv  ## set inverse value       
        getinv <- function() inv ## get inverse value       
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve computes the inverse of a matrix converted into a special list
## format using the makeCacheMatrix function. If the inverse has already been 
## calculated and the matrix has not changed, this function retrieves the 
## cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv() ## assigns a previously computed inverse to inv
        if(!is.null(inv)) { ## proceeds if inv previously computed
                message("getting cached data")
                return(inv) ## returns cached inverse
        }
        data <- x$get() ## gets matrix
        inv <- solve(data, ...) ## computes the inverse
        x$setinv(inv) ## sets the inverse to the computed value
        inv ## returns the inverse
}