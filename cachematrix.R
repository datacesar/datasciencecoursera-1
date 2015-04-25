## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setMatrix <- function(my_matrix)
        {
                x <<- my_matrix
                m <<- NULL
        }
        getMatrix <- function()
        {
                x
        }
        
        
        setSolve <- function(solve)
        {
                 m <<- solve
        }
        getSolve <- function()
        {
                m
        }
        
        list(setMatrix = setMatrix, getMatrix = getMatrix, setSolve = setSolve, getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) 
        {
                message("getting cached data")
                return(m)
        }
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
