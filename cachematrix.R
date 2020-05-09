
#Author: Jadwiga Szkatuła 
#Following functions allows save calculating resources and do not inverse the matrix if 
#its elements does not change and inverse was previously calculated

## The first function makeCachematrix is creating a "special" matrix, 
#it contains 4 funtions to set and get this matrix and get and set its inverse.


makeCacheMatrix <-function(x=matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

##cacheSolve first is checking if the inversion was calculated. If so, it gets the inverse
#from the cache and skips the computation. Otherwise, it calculates the inverse matrix of
#the data and sets the value of the inverse in the cache via the setinv function

cacheSolve<-function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- inv(data, ...)
    x$setinv(m)
    m
}