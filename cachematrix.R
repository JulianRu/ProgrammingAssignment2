## These two functions try to save computing again the invertion 
## of a matrix which can be a quite resource demanding operation

## We create a list in which the inverse can be stored

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inver <<- inverse
    getInverse <- function() inver
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## If the inverse has already been calculated it should bring th e inverse from the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inver = x$getInverse()
    if (!is.null(inver)) {
        message("getting cached data")
        return(inver)
    }
    mat <- x$get()
    inver <- solve(mat, ...)
    x$setInverse(inver)
    inver
}
