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
    set_inverse <- function(inv) inv_matrix <<- inv
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
        
    inv_matrix <- x$get_inverse()
    
    matrix_to_inverse <- x$get()
    inv_matrix <- solve(matrix_to_inverse)
    x$set_inverse(inv_matrix)
    
    inv_matrix
}
