## Put comments here that give an overall description of what your
## functions do

## This function keeps all the cached matrix. If the requested matrix have any cached inverse matrix then it returns the inverse matrix else return null

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
    
}


## This function makes use of makeCacheMatrix. It first checks if inverse is available and if not then it calculates the inverse and save it into cache using the same makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse_matrix <- x$getinverse
    if(!is.null(inverse_matrix)){
        message("getting cached data")
        return(inverse_matrix)
    }
    
    original_matrix <- x$get()
    inverse_matrix <- solve(original_matrix,...)
    x$setinverse(inverse_matrix)
    inverse_matrix
}





