

## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
    message("x is ", class(x))
    message("x size", nrow(x), ncol(x))
    message(summary(x))
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
