## The assignment includes two functions to cache 
## the inverse of a matrix

## Test code:
## A <- matrix(c(1, 0, 0, 0, 0, 1, 0, 1, 0), nrow = 3, ncol = 3)
## B <- makeCacheMatrix(A)
## cacheSolve(B)
## cacheSolve(B)

## The first function creates a special "matrix" object 
## that can cache its inverse.
## The first function creates a list containing a 
## function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## Set the value of the matrix
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        ## Get the value of the matrix
        get <- function() x
        ## Set the value of the inverse of the matrix
        setinv <- function(solve) inv <<- solve
        ## Get the value of the inverse of the matrix
        getinv <- function() inv
        list( set = set, get = get,
              setinv = setinv,
              getinv = getinv)
        
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## It first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache 
## and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and 
## sets the value of the inverse in the cache via the 
## setinv function.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        ## Check the cached inverse
        inv <- x$getinv()
        ## Return the cached inverse if it exists.
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        ## Caculate the inverse if it does not exist 
        ## and store the calculation result.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
