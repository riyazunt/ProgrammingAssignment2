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