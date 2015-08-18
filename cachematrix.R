##Calculate the inverse of a matrix and save it for future use.

##makeCacheMatrix will create a matrix and save its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(get=get,setinverse=setinverse,getinverse=getinverse)
    
}


## cacheSolve will return a matrix that is the inverse of 'x'. 
##If it's been calculatd before, it will return the result in cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
