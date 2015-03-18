## The following functions borrow heavily from the example,
## Caching the Mean of a Vector, usedd for this assignment.
##
## These two functions provide a means to cache the inverse
## of an invertible matrix if it has already been calculated.
## If not, it will calculate the inverse and store it in cache.
## 
## For example:
##
## > z <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
## 
## The first time we call cacheSolve:
##
## > cacheSolve(z)
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
##
## we get a result that has been calculated. However,
## when we call it again, the inverse matrix is retrieved
## from cache:
##
## > cacheSolve(z)
## getting cached data
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1


## This function, makeCacheMatrix, creates a special "matrix", 
##  which is really a list containing a function to:
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The second function, cacheSolve, calculates the inverse of
## the special "matrix" created with the first function. 
## However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the 
## inverse of the data and sets the value of the inverse in 
## the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)%*%data  
        x$setinverse(inverse)
        inverse
}
