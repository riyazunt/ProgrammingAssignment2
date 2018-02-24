# This function creates a special "MATRIX" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
        invr <- NULL
        set <- function(y) 
        {
                x <<- y
                invr <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invr <<- inverse
        getInverse <- function() invr
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# This function computes the inverse of the special "MATRIX" created by makeCacheMatrix above.
# If the inverse is already calculated (and if the matrix is not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        invr <- x$getInverse()
        if (!is.null(invr))
        {
                message("getting cached data...")
                return(invr)
        }
        mtrx <- x$get()
        invr <- solve(mtrx, ...)
        x$setInverse(invr)
        invr
}
