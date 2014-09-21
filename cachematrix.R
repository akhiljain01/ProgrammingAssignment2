## The file has two functions makrCacheMatrix() and cacheSolve().
## The two functions together provide the functionality to calculate the inverse of a square matrix
## If the inverse of a matrix was calculated earlier and is available in the cache, then that inverse
## is returned without recalculating the inverse of the matrix. If the result is not in cache, then the 
## inverse is calculated. The calculated result is cached before returning to the user.

## makeCacheMatrix: 
##              Accepts a matrix and stores it in the environment. 
##              Also, stores the inverse of a square matrix. 
##              Data (matrix or inverse) can be retrieved or set by getter and setter methods. 
##              Arguments - A matrix, Returns - a list of setter and getter methods
makeCacheMatrix <- function(x = matrix()) {
        
        xInverse <- NULL
        
        set <- function(y){
                x <<- y
                xInverse <- NULL
        }
        get <- function() x

        setInverse <- function(inverse) xInverse <<- inverse
        getInverse <- function() xInverse
        
        ##returns the list of methods that the makeCacheMatrix provides
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: 
##              Returns the inverse of a square matrix.
##              This method first checks if the result is already cached for input matrix. 
##                 Returns the cached result, if it has; 
##                 Otherwise, calculates, caches and returns the calculated result.

cacheSolve <- function(x, ...) {
 
        inverse <- x$getInverse()

        ##checks if the inverse matrix was in cache or not.
        ##if the number of rows is null, that means the inverse was not cached
        if(!is.null(nrow(inverse))){
                message("getting cached data")
                return(inverse)
        }
        
        message("calculating the data")
        data <- x$get()
        inverse <- solve(data)
        
        ##Caching the data
        x$setInverse(inverse)
        inverse
}
