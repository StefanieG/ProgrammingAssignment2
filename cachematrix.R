## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and
## there may be some benefit to caching the inverse of a matrix
## rather than compute it repetedly. 

## This file containts two functions: 1. 'makeCacheMatrix()' and 2. 'cacheSolve'.

## 1. makeCacheMatrix(): It creates a special 'matrix' object
## that can cache its inverse.
       
makeCacheMatrix <- function(x = matrix()) {
        ## 'x' - square invertible matrix
        ## Returns a list containing functions to:
        ## 1. set the matrix
        ## 2. get the matrix
        ## 3. set the inverse
        ## 4. get the inverse
        ## The list is used as input for cacheSolve().

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                ## use "<<-" to assign a value to an object in an environment
                ## different from the current environment
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## 2. cacheSolve(): This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix(). 
## If the inverse has already been calculated, it retrieves it from the cache.

cacheSolve <- function(x, ...) {
        ## 'x' output of makeCacheMatrix()
        
        inv <- x$getinverse()
        
        ## if the inverse has already been calculated:
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## if not, calculates the inverse:
        matrix.data <- x$get()
        inv <- solve(matrix.data, ...)
        
        ## sets the value of the inverse in the cache with setinverse()
        x$setinverse(inv)
        
        return(inv)
}

## Test case of the above function pair:
## A BIG THANK YOU to all active posters in the discussion forum!
## x <- matrix(1:4, ncol=2)
## a <- makeCacheMatrix(x)
## a$get()
        ##      [,1] [,2]
        ##[1,]    1    3
        ##[2,]    2    4

## No cache in the first run:
## > cacheSolve(a)
        ##      [,1] [,2]
        ## [1,]   -2  1.5
        ## [2,]    1 -0.5

## Retrieves from the cache the second time:
## > cacheSolve(a)
        ##getting cached data
        ##      [,1] [,2]
        ##[1,]   -2  1.5
        ##[2,]    1 -0.5