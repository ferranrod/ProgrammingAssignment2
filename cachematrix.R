# Matrix inversion is usually a costly computation and their may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly.

# Example:
# Create a matrix x:
# > x <- matrix(rnorm(25), nrow = 5)
# > m <- makeCacheMatrix(x)
# > m$get()
# Return the inverse, first run not cached
# > cacheSolve(m)
# Retriving from the cache in the second run
# > cacheSolve(m)


##
# Function: makeCacheMatrix
# Description: Creates a special "matrix" object that can cache its inverse.
# Parameters:
#  x: matrix to inverse
# Return a list of functions:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse matrix
# 4. Get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        # invmat will store the cached inverse matrix
        invmat <- NULL
        
        # Setter for the matrix
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        
        # Getter for the matrix
        get <- function() x
        
        # Setter for the inverse matrix
        setInverse <- function(solve) invmat <<- solve
        
        # Getter for the inverse matrix
        getInverse <- function() invmat
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        invmat <- x$getInverse()

        # If the inverse of matrix is already calculated, return it
        if(!is.null(invmat)) {
                message("Getting cached data.")
                return(invmat)
        }
        
        # The inverse is not yet calculated
        matrix <- x$get()
        invmat <- solve(matrix, ...)
        
        # Cache the inverse matrix
        x$setInverse(invmat)
        
        # Return it
        invmat
}
