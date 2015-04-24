## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## These functions will cache the inverse of a matrix.


## This function will create a matrix that can cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x<<-y
                m<<-NULL
        }
        get <- function() x
        setinverse <- function(solve) m<<-solve
        getinverse <- function() m
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## This function will first check if the inverse has been cached. 
## If yes, it retrieves the inverse; 
## if not, it computes the inverse and return it.

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
