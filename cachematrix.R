## These two functions are to compute matrix inversion. 
## They are caching the inverse of a matrix rather than compute it repeatedly.
## At the he first time you need to compute the inverse of the matrix
## After that, you only retrive the inverse from the cache, no need to compute it again (if the matrix has not changed)

## This makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()){
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    ## return a list containing four functions
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

## This cacheSolve solves the inverse of the special "matrix" with the above function.
## It first checks to see if the inverse has already been solved.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it solves the inverse of the matrix, and sets the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...){
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    ## if the inverse hasn't been solved, then solve it and cache via the setinverse function.
    x$setinverse(i)
    i
}