#Write the following functions:

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
#should retrieve the inverse from the cache.



## Creates a matrix object and returns a list with 4 functions(set(), get(), set_invers(), and get_inverse()). Also, initializes the inverse variable(i)
##in the parent environment

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL               
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) i <<- inverse
        get_inverse <- function() i
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## Takes a makeCacheMatrix list object and returns/calculates the inverse of the matrix element.  If the inverse has already been calculated for
## that specific matrix the inverse is retrieved from the parent environment and not calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$get_inverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$set_inverse
        i
}
