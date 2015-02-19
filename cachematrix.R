## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #set mean to NULL as place holder
        set <- function(y) { #defines a function to cache x to y and cache mean to null
                x <<- y
                m <<- NULL
        }
        get <- function() x #returns x
        setSolve <- function(solve) m <<- solve #sets solve, m to solve
        getSolve <- function() m #returns m
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve) # returns 
        #the matrix containing all functions just defined
}


## Write a short comment describing this function

## CacheSolve is a function that computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated, then cache solve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve() #assign to m in cachemean, the value from getSolve(m) that is NULL
        if(!is.null(m)) { # if the mean stored under the parametersx is not null return message
                message("getting cached data")
                return(m)
        }
        data <- x$get() #assign to data the matrix x
        m <- solve(data, ...) #Calculate inverse matrix and assign to m
        x$setSolve(m) #Store the matrix m under the parameters matrix x
        m#return m
}
