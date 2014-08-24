
## This function, makeCacheMatrix creates a special "matrix", which 
##is really a list containing a function to:

## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse matrix
## 4.get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function creates an inverse of the special "matrix" 
## created with the above function. However, it first checks to see if 
## the inverse has already been created. If so, it gets the inverse matrix 
## from the cache and skips the creation. Otherwise, it creates the 
## inverse of the matrix and sets the inverse matrix in the cache via 
## the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}