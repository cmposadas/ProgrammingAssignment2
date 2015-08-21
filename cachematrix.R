## The combination of the two functions below will produce the inverse of a specified matrix 
## where the inverse can be cached and therefore, re-computation of the inverse matrix is not
## required.

## The makeCacheMatrix function will create a list of functions for the matrix specified 
## in the argument that will:
## 1. Set the values of the matrix
## 2. Get the values of the matrix
## 3. Allow the inverse of the matrix to be cached
## 4. Retrieve the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function returns the inverse of the matrix specified in the
## makeCacheMatrix function. To avoid redundant computations, this function will first check
## to see if the inverse of the matrix has already been produced. If the inverse has already
## been produced, then the inverse computation will get bypassed and the cached inverse
## matrix will be retrieved.

## If the inverse matrix has not yet been produced, this function will compute the inverse 
## and cache it for future use.

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