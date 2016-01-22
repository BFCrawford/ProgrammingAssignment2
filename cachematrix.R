## Assignment 2: Caching the Inverse of a Matrix:

## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly. 
## Below are a pair of functions that are used to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        Set_Inverse <- function(inverse) inv <<- inverse
        Get_Inverse <- function() inv
        list(set = set, get = get,
             Set_Inverse = Set_Inverse,
             Get_Inverse = Get_Inverse)
}

## This function calculates the inverse of the special "matrix" created by the makeCacheMatrix function above. 
## The function first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$Get_Inverse()
        if (!is.null(inv)) {  
                message("retrieving the inverse from the cache")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$Set_Inverse(inv)
        inv
}
