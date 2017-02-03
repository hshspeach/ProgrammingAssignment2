## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The following function creates a special "matrix" object that can cache the input matrix and its inverse

makeCacheMatrix<- function(x = matrix()) {
        inverse<- NULL
        set<- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get<- function() x
        setinv<- function(inv) inverse <<- inv
        getinv<- function() inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
##then cacheSolve should retrieve the inverse from the cache.
##Basically it calculates the inverse of the data and sets the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        mat <- x$get()
        inverse <- solve(mat, ...)
        x$setinv(inverse)
        inverse
}
