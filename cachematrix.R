
## This functions help to work more efficient, as they cache the inverse matrix. If cacheSolve is executed, 
## and a result is alreay cached (i is not NULL), the operation will not start again but the cache result will be returned.

## The Function makeChacheMatrix 
##1. sets the values of the matrix
##2. gets the values of the matrix
##3. sets the result of the inverse matrix
##4. gets the inverse matrix 


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve  computes the inverse of the special "matrix" returned by makeCacheMatrix above
## then the cache value is used.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i        
}
