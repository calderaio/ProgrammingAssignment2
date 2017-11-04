## The following 2 functions help to invert a matrix using caching, by taking the lexical scoping of R.
## Since matrix inversion is a costly computation, it makes sense caching the inverse instead of caluclate it repeatedly 
## the first function creates an R object that stores the inverted matrix, while the second one requires the object returned
## to retrieve the inverted matrix from the cached value stored in the object retrurned. See more comments below.

## This function expects a matrix with the same number of columns and rows
## The set function assigns the input argument y to x in the parent 
## environment and assigns NULL to m, in order ot clear any cached value


makeCacheMatrix <- function(x = numeric()) {
        ## m is initialized
        m <- NULL
        ## The set function assigns the input argument y to x in the parent 
        ## environment and assigns NULL to m, in order to clear any cached value  
        ## using the <<- operator, the value will be stored in the parent environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## all this function does is returning the variable x
        get <- function() x
        ## here the inverted matrix m is defined
        setsolve <- function(solve) m <<- solve
        ## and this one returns the m value
        getsolve <- function() m
        
        ## a new object is created here for all the functions defined above
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function is designed to retrieve the required value from the function designed above.

cacheSolve <- function(x, ...) {
        
        ## this function gets the calculated matrix from the function above. if it has not yet been cached m will be NULL
        m <- x$getsolve()
        ## if m has already been calculated, the data wil be returned directly and a message is displayed
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if the matrix needs to be inverted, it is bein done by getting the data from the object and applying the solve function
        data <- x$get()
        m <- solve(data, ...)
        ## the calculated matrix is set to the object to be used later
        x$setsolve(m)
        ## the result is returned
        m
}
