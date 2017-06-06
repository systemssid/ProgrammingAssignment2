# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute 
# it repeatedly. The following pair of functions cache the inverse of a matrix.

makeCacheMatrix <- function(X = matrix()) {
        inv <- NULL
        set <- function(y) {
                X <<- y
                inv <<- NULL
        }
        get <- function() X
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(X, ...) {
        inv <- X$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- X$get()
        inv <- solve(data, ...)
        X$setInverse(inv)
        inv
}
