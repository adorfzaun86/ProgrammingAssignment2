## makeCacheMatrix creates a special matrix object that can cache its inverse; 
## caching is a way to avoid re-evaluating long running computations

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## computes the inverse of the special matrix returned by the function above.  
## this cacheSolve function will retrieve the inverse from the cache; this is 
## used when the inverse has already been calculated and if the matrix has 
## not changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message ("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- inverse(data, ...)
        x$setinverse(inv)
        inv
}
