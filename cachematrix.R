################################################################################
## Author      : Arjun Rai Mahendra
## Project     : Programming Assignment 2 - Lexical Scoping
## File        : cachematrix.R
## R Version   : 3.1.2
## Description : The objective of this script is to compute the inverse of a 
##               non-singular, invertible square matrix, and cache the inverse
##               of the matrix, so as to retrieve without computation if the 
##               inverse is calculated on the same matrix again.
################################################################################

#-------------------------------------------------------------------------------
## `makeCacheMatrix`: This function creates a special "matrix", which is really 
##  a list containing a function to -
##     1) `get` - get the value of the matrix
##     2) `setinv` - set the value of the inverse
##     3) `getinv` - get the value of the inverse
#-------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
        
        ## Variable to cache the inverse of the matrix
        inv <- NULL
        
        ## Function to get the matrix
        get <- function() x
        
        ## Function to set the inverse
        setinv <- function(inverse) inv <<- inverse
        
        ## Function to get the value of the inverse
        getinv <- function() inv
        
        ## Return the list of functions defined above
        list(get = get,                                                               
             setinv = setinv,
             getinv = getinv)
}


#-------------------------------------------------------------------------------
## `cacheSolve` : The following function calculates the inverse of the special
## "matrix" created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse 
## of the data and sets the value of the inverse in the cache via the 
## `solve` function.
#-------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of special matrix 'x' received
        ## from makeCacheMatrix
                
        ## Get the cached value of the inverse of the matrix
        inverse <- x$getinv()
        
        ## Check if the inverse is indeed cached
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        
        ## If the inverse is not cached then calculate and cache the inverse
        data <- x$get()
        
        ## Assuming the matrix is invertible
        inverse <- solve(data, ...)
        x$setinv(inverse)
        
        ## Return the inverse calculated
        inverse
}
