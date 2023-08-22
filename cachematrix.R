#The function caches the inverse of a matrix object

makeCacheMatrix <- function(x = matrix()) {
  
                inv <- NULL   # NULL for inverse initialization
                set <- function(y) {
                                x <<- y
                                inv <<- NULL
                }
                
                get <- function() x # retrieve matrix x
                setinverse <- function(inverse) inv <<- inverse
                getinverse <- function() inv # retrieve the matrix inverse
                list(set = set, get = get,
                    setinverse = setinverse,
                    getinverse = getinverse)
}


#The function computes the matrix inverse returned by makeCacheMatrix.
#If the inverse has already been computed, then cacheSolve fetches the inverse
#from the existing cache.

cacheSolve <- function(x, ...) {
                inv <- x$getinverse()
                if(!is.null(inv)) {
                                message("fetching cached data")
                                return
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$setinverse(inv)
                inv
  
}
