## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Sets a matrix to NULL and ge??? no idea look up

makeCacheMatrix <- function(x= matrix()) { #matrix() or numeric() ??
    #makeCacheMatrix <- function(x = numeric()) {
    matrix_ <- NULL
    set <- function(y) {
        x <<- y
        matrix_ <<- NULL
    }
    get <- function() x
    set_inverse <- function(solve) matrix_ <<- solve
    get_inverse <- function() matrix_
    list(set = set, get = get, set_inverse = set_inverse, 
         get_inverse = get_inverse)
    
}



## Write a short comment describing this function
## Looks for matrix in cache. If it has not changed
## gets it from makeCacheMatrix. returns the inverse of matrix x.
## If it has changed calculates inverse from there.

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    matrix_<- x$get_inverse()
    if (!is.null(matrix_)) {
        message("getting cache matrix")
        return(matrix_)
    }
    data <- x$get() #if cached matrix is NULL uses new x to set it
    matrix_ <- solve(data, ...) ## calculates the inverse of the matrix
    x$set_inverse (matrix_)
    matrix_
}
