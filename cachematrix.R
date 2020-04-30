## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function, makeCacheMatrix create a special "matrix", which is 
## actually a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Write a short comment describing this function
## The following function cache the inverse of the special "matrix" created 
## with the above function
## It first checks to see if the matrix inverse has already been get.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculate the inverse of the matrix and sets the inverse
## of the matrix in the cache via the cacheSolve function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix_data <- x$get()
        m <- solve(matrix_data, ...)
        x$setinverse(m)
        m
}
## Return a matrix that is the inverse of 'x'