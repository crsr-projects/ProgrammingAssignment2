## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        
        getinverse <- function() inv
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Computes the inverse of the special matrix
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

m <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
cacheSolve(m)   
cacheSolve(m)   