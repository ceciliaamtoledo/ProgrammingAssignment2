##R Programming - Programming Assignment 2
##Author: Cecilia A.M. Toledo

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x=matrix()){
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve return the inverse of matrix 'x'
cacheSolve <- function(x, ...){ 
        ##Search if the inverse is already in cache and retrieve from the cache
        m <- x$getsolve()       
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If not, it will  and returns the inverse from data
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m    
}