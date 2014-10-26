## Put comments here that give an overall description of what your
## functions do
## The functions create a cache the contains a matrix and its inverse as well as
## calculate the inverse (if needed) and storing it in the cache. 


## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special "Cached Matix", 
## which is really a list containing a function to
## set the value of the cached matrix
## get the value of the cached matrix
## setinv the value of the cached inverse of the Matrix
## getinv the value of the cached inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(theinv) inv <<- theinv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## The next function calculates the inverse of the special "cached matrix" created 
## with the above function. However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
##  in the cache via the setinv function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	   inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
