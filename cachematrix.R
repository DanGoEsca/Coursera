##The function makeCacheMatrix is declared in such a way as to cache its inverse and cacheSolve obtain the inverse of the matrix.


########## makeCacheMatrix #########
## The finction makeCacheMatrix create a matrix that can cache its inverse.
## set-> Changes the matrix stored in the main function
## get-> Returns the matrix stored in the main function
## setinv, getinv-> Assign the value of the input in the variable previously declared as m in the main function and return it

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL    
                
} 
get <- function() x
setinv <- function(solve) m <<- solve 
getinv <- function() m
list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)         
}


############ cacheSolve #########
## Computes the inverse of the matrix previously declared, if the inverse has not been calculated,
#the data gets the matrix stored with makeCacheMatrix, m computes the inverse else if the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the inverse of the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
           message("getting cached data")
           return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
