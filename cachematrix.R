## Matrix inversion is usually a costly computation. These two functions are 
## used to put in cache the inverse of a matrix. So if we have to inverse a 
## matrix that we have already inversed, we will access to the cache instead of
## repeating the inversion.


## This function creates a special matrix which is really a list containing 
## a function to :
##    1. Set the value of the matrix
##    2. Get the value of the matrix
##    3. Set the value of inverse of the matrix
##    4. Get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        ## Variable for the cached inverse matrix
        inv <- NULL
        
        ## 1. Set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## 2. Get the value of the matrix
        get <- function() x
        
        ## 3. Set the value of inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        
        ## 4. Get the value of inverse of the matrix
        getinverse <- function() inv
        
        ## return the list of functions
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of the matrix, assuming that the matrix 
## supplied is always invertible. If the matrix has already been inversed, 
## it returns the cached data (without any computation). Else, it computes 
## the inverse of the matrix and sets the value in the cache (with the 
## setinverse function in makeCacheMatrix).
cacheSolve <- function(x, ...) {
        ## Get the inverse matrix in makeCacheMatrix
        inv <- x$getinverse()
        
        ## If the matrix has already been inversed, it returns the cached data 
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## Else, it computes the inverse of the matrix (with the solve function)
        ## and sets the value in the cache with the setinverse function in 
        ## makeCacheMatrix. Then, it returns a matrix that is the inverse of x
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
