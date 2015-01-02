#########################################################################
## Programming Assignment 2                                             #
## Write the following functions:                                       #
## 1. makeCacheMatrix: This function creates a special                  #
##    "matrix" object that can cache its inverse.                       #
##                                                                      #
## 2. cacheSolve: This function computes the inverse of the special     #
##    "matrix" returned by makeCacheMatrix above. If the inverse        #
##    has already been calculated (and the matrix has not changed),     #
##    then the cachesolve should retrieve the inverse from the cache.   #
##                                                                      #
## Computing the inverse of a square matrix can be done with the solve  #
## function in R. For example, if X is a square invertible matrix,      #
## then solve(X) returns its inverse.                                   #
#########################################################################

makeCacheMatrix <- function(x = matrix()) { 
        
        m <- NULL 
        
        ##y <- NULL
        
        set <- function(y) { 
                
                x <<- y 
                
                m <<- NULL 
        }
        
        get <- function() {x }
        
        setsolveX <- function(solve) m <<- solve
        
        getsolveX <- function() m
        
        list(set = set, get = get,
             
             setsolveX = setsolveX,
             
             getsolveX = getsolveX) 
} 

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x' 
        
        m <- x$getsolveX()
        
        if(!is.null(m)) {
                
                message("getting cached data")
                
                return(m) 
        } 
        
        data <- x$get()
        
        x$set(data) 
                
        m <- solve(data, ...)
        
        x$setsolveX(m)
                
        m
        
} 
## For assessment...
## Compute the inverse of the square matrix; that is a 2 x 2 matrix
## > x <- matrix(data = c(4, 2, 7, 6), nrow = 2, ncol = 2)
## > y <- makeCacheMatrix(x)
## > cacheSolve(y)
## Please note....
## ... other values for the vector 'c(4,2,7,6)' in the matrix, can be used
## example: x <- matrix(data = c(14, 42, 7, 9), 2, 2)
## if you receive an error message..refresh page by
## Restart R in 'Session' menu of the R Console
## ...Thanks.