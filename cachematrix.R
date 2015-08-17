## Following is   a pair of functions that cache the inverse of a matrix.

#  This function creates a special "matrix" object that can cache its inverse. 
# this takes an invetribale matrix as input and creates a list with four functions 
# as output

makeCacheMatrix <- function(x = matrix()) {
        
        matinv <- NULL
        set <- function(y=matrix()) {
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        setmat <- function(solve) matinv <<- solve
        getmat <- function() matinv


# Following list is the output of this function 
#         set the value of the input matrix
#         get the value of the input matrix
#         set the value of the inverse matrix
#         get the value of the inverse matrix
        
        list(set = set, get = get,
             setmat = setmat,
             getmat = getmat)
        

}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
# Return a matrix that is the inverse of 'x'
        
        
        matinv <- x$getmat()
# If the inverse is already populated, then data will be read from cache 
        
        if(!is.null(matinv)) {
                message("getting cached data")
                return(matinv)
        }
        
        data <- x$get()
        
# To inverse the matrix provided as input
        matinv <- solve(data, ...)
        
        x$setmat(matinv)
        matinv
}
