makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the inverse property
    inv <- NULL
    
    ## Setter
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Getter
    get <- function() x
    
    ## Method to set the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    ## Method to get the inverse
    getinverse <- function() inv
    
    ## Return an R list object with the methods
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    ## Get inverse of x
    m <- x$getinverse()
    
    ## Check if it is cached
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Get the actual matrix from x
    data <- x$get()
    
    ## Inverse matrix
    m <- solve(data, ...)
    
    ## Set the inverse of x
    x$setinverse(m)
    
    ## Return inversed matrix
    m
}