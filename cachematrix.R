## The pair of functions will return the inverse of an invertible square matrix 
## and cache/retrieve that inverse to avoid a repetition of the calculation.

makeCacheMatrix<-function(x=matrix()) {
        ## Creates a special "matrix" object that can cache its inverse.
        ## (to be used in conjuction with 'cacheSolve')
        ## Outputs a list of 4 functions :-
        ##   'set' allows you to input a (square) matrix
        ##   'get' retrieves the matrix
        ##   'setinverse' will cause a search for an existing result of the inverse in parent environment
        ##   'getinverse' retrieves this result
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

cacheSolve <- function(x,...) {
        ## Returns the inverse of the special 'matrix' created with 'makeCacheMatrix'
        ## If the inverse has already been calculated it will retrieve the previous result from the cache instead.
        ## argument : x  a list of 4 (output from 'makeCacheMatrix' where the matrix
        ## to be inverted has been input via 'set')
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        uncached_matrix <- x$get()
        inv <- solve(uncached_matrix)
        x$setinverse(inv) 
        inv
}