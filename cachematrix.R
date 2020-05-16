## Put comments here that give an overall description of what your
## functions do

## The function, `makeCacheMatrix` creates a special "matrix",
## which is really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse the matrix
## 4.  get the value of the inverse the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solveMatrix) inv <<- solveMatrix
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse, getInverse = getInverse)
}


## The following function calculates the inverse of the special
## "matrix" created with the above function. However, it first
## checks to see if the inverse has already been calculated. If
## so, it `get` the inverse from the matrix cache and skips the
## computation. Otherwise, it calculates the inverse of the data
## and sets the value of the inverso in the cache via the
## `setinverse` function.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
