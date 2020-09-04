## My functions set and store a matrix. The inverse is then calculated and cached, 
## allowing the inverse to be retrieved without having to calculate it again. 


## This function defines the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inverse) {inv <<- inverse}
        getinverse <- function() {inv}
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function determines whether or not the inverse has been cached. If so,
## the inverse is retrieved. If not, the solve function is applied to get the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message('getting cached data')
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
