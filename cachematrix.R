## This functions cache the inverse of a square matrix

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse
makeCacheMatrix <- function(X = matrix()) {
    M <- NULL
        set <-function(Y){
            X <<- Y
            M <<- NULL
        }
    get <- function() X
    setinverse <- function(solve) M <<- solve
    getinverse <- function() M
    list (set=set, get=get,
          setinverse=setinverse,
          getinverse=getinverse)
}
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(X= matrix(), ...) {
    M <- X$getinverse()
    if(!is.null(M)) {
        message("getting cached data")
        return(M)
    }
    matrix <- X$get()
    M <- solve(matrix, ...)
    X$setinverse(M)
    M
}
