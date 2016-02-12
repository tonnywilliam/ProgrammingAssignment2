## create a mechanism to cache the inverse of matrices, so that you do not need to perform computation intensive matrix inversing operations repeatedly on the same matrix

## makeCacheMatrix defines the method to cache the inverse of a matrix (through setinverse), together with three other functions
## it sort of create a class CacheMatrix with four methods
## set/get/setinverse/getinverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL	# m is like a global variable in the makeCacheMatrix function scope
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## this function takes input x which is created by the makeCacheMatrix, it checks wether its inverse has being cached. if yes then it return the cached inverse, if not then it calculate the inverse and cache it, it also returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)		#this is where the cache happens
        m
}