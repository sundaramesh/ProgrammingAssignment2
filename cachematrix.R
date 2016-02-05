## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        ## makeCacheMatrix holds the list of functions that will do the perform the following
        ## 1. Setting the Straight Matrix value
        ## 2. Getting the Straight Matrix value
        ## 3. Setting the Inverse Matrix value
        ## 4. Getting the Inverse Matrix value

        strtCache <- NULL

        setStrtMatrix <- function(y) {
                x <<- y
                strtCache <<- NULL
        }
        
        getStrtMatrix <- function() { 
                x
        }
        
        setInvMatrix <- function(invCache) strtCache <<- invCache
        
        getInvMatrix <- function() {
                strtCache
        }
        
        list(setStrtMatrix = setStrtMatrix, getStrtMatrix = getStrtMatrix, 
             setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
        
}
        


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

        # get the cached value
        invMatrix <- x$getInverse()

        # if a cached Inverse Matrix is not NULL the return
        if(!is.null(invMatrix)) {
                return(invMatrix)
        }

        # Calulate the Inverse Matrix and store it in Cache
        
        cacheData <- x$getMatrix()

        # Solve the Cached value store back in Inverse Matrix
        invMatrix <- solve(cacheData)
        
        # Extract the Inverse Cache
        x$cacheInverse(invMatrix)
        
        # Return the Innverse Matrix
        invMatrix
}
