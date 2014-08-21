## Two functions that cache inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        
        
        m <- NULL
        ##setting the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ##getting the matrix
        get <- function() {
                x
        }
        ##setting the inverse
        setInverse <- function(inverse) {
                m <<- inverse
        }
        ##getting the inverse
        getInverse < function() {
                m
        }
        ##returns list of methods
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)


}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ##if inverse matrix already present in the cache -> return without calculating
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##if not
        data <- x$get()
        ##find inverse matrix by matrix multiplication
        m <- solve(data, ...)
        x$setInverse(m)
        ##prints out inverse matrix
        m
}
