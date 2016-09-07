## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Below function will take invertible matrix and set the inverse manually

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        #setinverse vector will inverse the matrix
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## The below function take matrix argument and checks if it was inversed or not, else it will inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
