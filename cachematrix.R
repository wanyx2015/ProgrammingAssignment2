## cacheSolve() is a revised version of solve() which can cache the inverse

## makeCacheMatrix is used to store the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## Set the matrix, and clear the existing inverse
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    ## Get the matrix
    get <- function() x
    
    ## Cache the inverse
    setsolve <- function(inverse) m <<- inverse
    
    ## Get the inverse
    getsolve <- function() m
    
    list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## Solve a matrix and cache it
## Next time just return it without solving it again

cacheSolve <- function(x, ...) {
    
    ## Retrive the inverse of the matrix
    m <- x$getsolve()
    
    ## Check if it is already calculated
    if(!is.null(m)){
        message("getting cached matrix inverse.")
        return(m)
    }
    
    ## Inverse is not calculated, let's calculate
    data <- x$get()
    m <- solve(data)
    
    ## Cache the inverse for future use
    x$setsolve(m)
    
    ## Return the inverse
    m        
}
