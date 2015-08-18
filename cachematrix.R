#creates a special "matrix" object that can cache its inverse.
#stores functions that will be used by cacheSolve.

makeCacheMatrix <- function(x= matrix()) { 
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


##computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##First checks if the inverse is found stored. 
##If it finds it (and the matrix has not changed),
##it retrieves it from cache (with a message).
##Otherwise, it calculates it and sets the inverse in cache.

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    matrix_<- x$get_inverse()
    ##searches for 'matrix_' (inverse of 'x') in the cache. 
    ##If it finds it returns a message and the result stored.
    if (!is.null(matrix_)) { 
        message("getting cache matrix")
        return(matrix_)
    }
    data <- x$get() ##if cached 'matrix_' is NULL or different gets it
    matrix_ <- solve(data, ...) ##and calculates the inverse of 'x'
    x$set_inverse (matrix_) ##then sets the 'matrix_' to the new value
    matrix_ ##and returns it
}
