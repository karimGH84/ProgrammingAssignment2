## Put comments here that give an overall description of what your
## functions do

## utility of this function is making a matrix into a cache mode 

makeCacheMatrix <- function(x = matrix()) {
        inverse<- NULL
        set <- function(y) {
                x <<- y
                inverse<<- NULL
        }
        get <- function() x
        set_inverse <- function(inv) inverse<<- inv
        get_inverse <- function() inverse
        list(set = set,
        get = get,
        set_inverse = set_inverse,
        get_inverse = get_inverse
	  )
}


## this fuction return the value of inverse of matrix if this value not saved
## or the value saved

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<- x$get_inverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        m <- x$get()
        inverse<- solve(m, ...)
        x$set_inverse(inverse)
        inverse
}