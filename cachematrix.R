## This program performs caching the Inverse of a Matrix

## makeCacheMatrix function creates a special matrix object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverseMat <- NULL
        set <- function(y) {
                x <<- y
                inverseMat <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inverseMat <<- solve
        getsolve <- function() inverseMat
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix
## function. If the inverse has already been calculated, then the cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        inverseMat <- x$getsolve()
        if(!is.null(inverseMat)) {
                message("getting cached data")
                return(inverseMat)
        }
        data <- x$get()
        inverseMat <- solve(data, ...)
        x$setsolve(inverseMat)
        inverseMat
}
