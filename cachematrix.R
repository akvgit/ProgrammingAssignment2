## The below 2 functions can be used to cache the inverse of a square matrix
## Example usage
## a <- matrix(c(3,1,2,1),nrow=2, ncol=2)
## makematrix <- makeCacheMatrix(a)
## cacheSolve(makematrix)

## Create a special matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)      
}


## Determines the inverse of the square matrix
## if the inverse already calculated, this function will retrieve it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("Getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
