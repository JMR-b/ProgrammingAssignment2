# Programming Assignment 2 for R Programming on Coursera.

# Assignment: Caching the Inverse of a Matrix : In this second programming assignment, 
# we create an R function that is able to cache potentially time-consuming computations. 
# We also will take advantage of the scoping rules of the R language 
# and how they can be manipulated to preserve state inside of an R object.

# 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse by
# setting and getting the value of the vector, setting and getting the value of the mean.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


# 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed),then cacheSolve should retrieve
# the inverse from the cache.To Compute the inverse of a square matrix we make use of  the solve function in R.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
