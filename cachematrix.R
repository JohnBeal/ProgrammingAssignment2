## Functions create a special "matrix" object and compute its inverse. Subsequent calls to cacheSolve will return the 
## cached value of the inverse if the matrix has not changed.
 

## Creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function () inv
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Computes inverse of matrix returned by makeCacheMatrix or retrieves previously calculated inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}-
