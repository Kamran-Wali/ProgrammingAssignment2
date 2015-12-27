## Put comments here that give an overall description of what your
## functions do

## Computing the inverse of a square matrix using the solve function
makeCacheMatrix <- function(x = matrix()) {
       ## This function will return a list of the following functions:
       ## set the value of the matrix
       ## get the value of the matrix
       ## set the value of the inverse matrix
       ## get the value of the inverse matrix
        
        ## Initialize the return 
        
        inv <- NULL
        set <- function(y) {
                ## Using the '<<-' variable to assign value to object different 
                ## from current environment
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function() inv
        list(set=set, get=get, 
             setinv=setinv, getinv=getinv)
}

## Caching the inverse of a square matrix
cacheSolve <- function(x, ...) {
       
        ## will cache and return inverse of the original matrix 
        ## input to makeCacheMatrix()
        
        inv <- x$getinv()
        
        ## Check if inverse has been calculated,
        ## If already calculated, return cache value
        
        if (!is.null(inv)){
                
                message("getting cached data")
                return(inv)
        }
        
        ## otherwise, calculate inverse 
        ## and then return
        
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        ## set the value of the inverse in cache
        x$setinv(inv)
        
        ## return the cached value
        return(inv)
}
