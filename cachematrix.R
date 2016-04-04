# This pair of functions can be used to cache the inverse of a matrix

# This function creates a special "matrix" object that can cache the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL
        set <- function(y){
                x <<- y
                ix <<- NULL
        }
        get <- function() x
        setinv <- function(inv) ix <<- inv 
        getinv <- function() ix
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix, above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        ix <- x$getinv()
        if(!is.null(ix)){
                message("getting cached data")
                return(ix)
        }
        data <- x$get()
        ix <- solve(data)
        x$setinv(ix)
        ix
}
