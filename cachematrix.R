## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function, makeCacheMatrix, creates a matrix, which is ##really a list containing a function to
##
##    set the value of the matrix
##    get the value of the matrix
##    setmatrix sets the value of the inverse matrix
##    getmatrix gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}

## Write a short comment describing this function
## cachesolve is a function that calculates the inverse of the
## matrix created with the above function. 
## It first checks to see if the inverse is already cached         ## (x$getmatrix()) . If so, it gets the inverse matrix from the  ## cache and skips the computation. Otherwise, it calculates the ## inverse matrix and sets that value in the cache via the
## setmatrix function.

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix <- x$get() 
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}