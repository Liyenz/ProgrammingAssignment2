## RProgramming - Assignment 
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.


makeCacheMatrix <- function(x = matrix()) { ## Create a matrix
    
    ## Define variable for cache
    inv <- NULL
    
    set <- function(y) {
        x <<- y ## Assign a value y to the variable matrix in parent environment. 
        inv <<- NULL
    }
    
    get <- function() x ## Return matrix x
    
    ## Set the cache inv equal to the inverse of the matrix x, and  get return inverse matrix x.
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of x
    inv <- x$getInverse()
    
    ## Check if inverse from cache has calculated.
    ## If result is not null, has calculaated, display meesage and return cache inv.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## If result is null, perform calculation. cache it x
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}