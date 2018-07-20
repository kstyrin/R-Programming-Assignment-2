## Put comments here that give an overall description of what your
## functions do

# Function makeCacheMatrix creates is applied to a square matrix X
# and creates four functions for saving to and retrieving from the memory 
# the values of matrix X and its inverse.
# Function cacheSolve takes as an inut the output of  
# makeCacheMatrix and returns the inverse of X either from the memory
# (if it was already computed and stored in the memory) or by computing it.

# A typical use of the two functions is the following:

# x <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
# xxinv <- makeCacheMatrix(x)
# xinv <- casheSolve(xxinv)

## Write a short comment describing this function

# This function takes as an argument a square matrix X and produces a list
# with four elements: 
# $setmat is a function that saves X
# $getmat is a function that retrieves X
# $setinv is a function that saves the inverse of X
# $getinv is a function that retrieves the inverse of X

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setmat <- function(y = matrix()) {
        x <<- y
        m <<- NULL
    }
    getmat <- function() {
        x
    }
    setinv <- function(xinv = matrix()) {
        m <<- xinv
    }
    getinv <- function() {
        m
    }
    list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function

# This function takes as an argument the list of four functions produced by
# function makeCacheMatrix applied to a square matrix of interest X.
# It first checks if the inverse of X was already computed by calling
# $getinv(). If so, the function takes the inverse of X from the memory without 
# recomputing it. Otherwise, it takes X from the memory by calling $getmat() and 
# computes the inverse of X

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    xmat <- x$getmat()
    xinv <- solve(xmat)
    x$setinv(xinv)
    xinv
}


