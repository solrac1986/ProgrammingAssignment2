## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates cacheable matrix to
## cacheSolve() function which sets and gets 

makeCacheMatrix <- function(x = matrix()) {

    inv_matrix <- NULL
    
    set <- function(y) {
        orig_matrix <<- y
        inv_matrix <<- NULL
    }
    
    get <- function() orig_matrix
    set_inverse <- function(solve) inv_matrix <<- solve
    get_inverse <- function() inv_matrix
    
    list(
        set = set, 
        get = get,
        set_inverse = set_inverse,
        get_inverse = get_inverse)  
}


## Write a short comment describing this function
## Computes the inverse of the cacheable matrix returned by makeCacheMatrix()
## If the inverse has already been calculated and there's no change in the matrix
## then the cacheSolve() returns the cached inverse

cacheSolve <- function(x, ...) {
        
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    
    inv <- solve(data, ...)
    
    x$setinv(inv)
    
    inv
}
