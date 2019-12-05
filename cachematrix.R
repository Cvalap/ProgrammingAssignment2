## Put comments here that give an overall description of what your
## functions do

## This function creates a "reverse array" object that you can cache

makeCacheMatrix <- function(x = matrix()) {
    I = NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) I <<- inverse
    getinverse <- function() I
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This function computes the inverse Matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    I <- x$getinverse()
    if(!is.null(I)){
        message("getting cached data")
        return(I)
    }
    data <- x$get()
    I <- solve(data,...)
    x$setinverse(I)
    I      
}


#Test functions
m <- matrix(1:4,2,2)
minv <- makeCacheMatrix(m)
w <- cacheSolve(minv)
