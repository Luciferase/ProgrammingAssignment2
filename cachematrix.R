## Below are two functions that cache the inverse of a matrix.

## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                               ## set m to NULL as place holder
        set <- function(y) {                    ## defines a function to cache x to y and cache m to
                x <<- y                         ## null
                m <<- NULL
        }
        get <- function() x                     ## assign get to return x
        setSolve <- function(solve) m <<- solve ## assign setSolve, cache m to solve
        getSolve <- function() m                ## assign getSolve to return m
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
        ## returns a list containing all functions just defined
}

## cacheSolve is a function that computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated, then cache solve prints out "getting  
## cached data " and retrieves the inverse from the cache instead.

cacheSolve <- function(x, ...) {
        m <- x$getSolve()                       ## assign to m in cacheSolve, the value from getSolve(m)
        if(!is.null(m)) {                       ## if the mean stored under the parameters x is not null 
                message("getting cached data")  ## return message "getting cached data"
                return(m)                       ## Return a matrix that is the inverse of 'x' from cache
        }
        
        data <- x$get()                         ## assign to data the matrix x
        m <- solve(data, ...)                   ## Calculate inverse matrix and assign to m
        x$setSolve(m)                           ## Store the matrix m under the parameters setSolve in x
        m                                       ## Return a matrix that is the inverse of 'x'
}
