## Below are two functions that are used to create a special object 
## that stores a matrix and caches its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is 
## really a list containing a function to
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of the matrix inverse
# 4) get the value of the matrix inverse

## To use these functions define an invertible matrix and run makeCacheMatrix
# on that matrix, assigning the output to an object. cacheSolve can then be 
# run on that output object to return the inverse of the original input matrix.
# Subsequent runs of cacheSolve for the same object should then be quicker.

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m.inv <- NULL
    set <- function(y) {
        x <<- y
        m.inv <<- NULL
    }
    get <- function() x
    setm.inv <- function(inverse) m.inv <<- inverse
    getm.inv <- function() m.inv
    list(set = set, get = get,
         setm.inv = setm.inv,
         getm.inv = getm.inv)
}

## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m.inv <- x$getm.inv()
    if(!is.null(m.inv)) {
        message("getting cached data")
        return(m.inv)
    }
    data <- x$get()
    m.inv <- solve(data, ...)
    x$setm.inv(m.inv)
    m.inv
}
