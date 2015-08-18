## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Sets a matrix to NULL and ge??? no idea look up

makeCacheMatrix <- function(x= matrix()) { #matrix() or numeric() ??
    #makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_inverse <- function(solve) m <<- solve
    get_inverse <- function() m
    list(set = set, get = get, set_inverse = set_inverse, 
             get_inverse = get_inverse)
    
}



## Write a short comment describing this function
## Looks for matrix in cache. If it has not changed
## gets it from makeCacheMatrix. returns the inverse of matrix x.
## If it has changed calculates inverse from there.

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    m<- x$get_inverse()
    if (!is.null(m)) {
        message("getting cache matrix")
        return(m)
    }
    data <- x$get() #if cached matrix is NULL uses new x to set it
    m <- solve(data, ...) ## calculates the inverse of the matrix
    x$set_inverse (m)
    m
}
