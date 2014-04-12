## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly
## We will use 2 functions makeCacheMatrix and cacheSolve to achieve this
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix creates a special "vector", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize inverse matrix as NULL
        inv <- NULL
        
        ## Define set function to set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Define get function to get the value of the matrix
        get <- function() {
                x
        }
        
        ## Define setinv function to set the value of the inverse
        setinv <- function(solve) {
                inv <<- solve
        }
        
        ## Define getinv function to get the value of the inverse
        getinv <- function() {
                inv
        }
        
        ## Return a list containing all 4 functions defined above
        list (set = set, get = get, setinv = setinv, getinv = getinv)
}

## The following function calculates the mean of the special "vector" created with the above 
## function. However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        
        ## get inverse from the special vector list using getinv function
        inv <-x$getinv()
        
        ## check if the inverse has alreay been calculated. If yes, then return the value of
        ## inverse from cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## The inverse hasn't been calculated yet. So get the value of matrix using get function
        data <- x$get()
        
        ## calculate the inverse using solve
        inv <- solve(data, ...)
        
        ## set the inverse using setinv function
        x$setinv(inv)
        
        ## return the value of inverse calculated above
        inv
}
