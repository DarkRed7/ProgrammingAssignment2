## The function makecacheMatrix creates a matrix
## similar to makeVector, it actually creates a list containing a functions
## the functions are:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {x}
        setinverse <- function(solve) {m <<- solve}
        getinverse <- function() {m}
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## The function makecacheMatrix creates a matrix
## similar to makeVector, it actually creates a list containing a functions
## the functions are:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix

## The function cacheSolve solves the inverse of the matrix
## created with the above function. However, it first checks 
## to see if the inverse has already been calculated. 
## If so, it gets the mean from the cache. Otherwise, it calculates
## the inverse of the matrix and sets the value of the inverse in the cache
## via the setinverse function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
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

