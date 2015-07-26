## makeCacheMatrix creates a list containing a function to set and get a matrix 'x' and its inverse
## The list created by makeCacheMatrix is the input for cacheSolve
## cacheSolve returns the inverse of matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
    ## makeCacheMatrix returns a list containing a function to:
        ## 1. Set the matrix
        ## 2. Get the matrix
        ## 3. Set the inverse of the matrix
        ## 4. Get the inverse of the matrix
    ## x is an invertible matrix
    
    inv = NULL
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
    ## cacheSolve returns a matrix that is the inverse of the original matrix 'x'
    ## x is the list returned by makeCacheMatrix
    
    inv = x$getinv()
    
    ## if inverse has already been calculated, retrieve from cache
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    ## if inverse has not been calculated, calculate it
    data <- x$get()
    inv <- solve(data, ...)
    
    ## set the value of the cached inverse, return the inverse
    x$setinv(inv)
    return(inv)
}