## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        prevMatrix <- NULL
        set <- function(y) {
                x <<- y
        }
        get <- function() x
        
        #function set inverse of matrix and remember the original matrix
        setInverse <- function(inv){
                m <<- inv 
                prevMatrix <<- x  
        } 
        
        #getting previos matrix
        getPrevMatrix <- function() prevMatrix  
        
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse,
             getPrevMatrix = getPrevMatrix
             )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
        m <- x$get()
        prevMatrix <- x$getPrevMatrix()
        if(!is.null(inv) && identical(m, prevMatrix) )  {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        message("Matrix inversion")
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
