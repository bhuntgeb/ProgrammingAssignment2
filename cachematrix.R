
## These two functions work together. The "makeCacheMatrix" function
## returns a special object. This object is a list of functions that
## get and set the values of the matrix and their inverse values.
## The environment of these functions also works as a cache.
## The "cacheSolve" function calculates the inverse of the stored matrix
## and stores it only if there is no inversion of the matrix in the cache.

## makeCacheMatrix:
## This function returns a special matrix object with several getter and setter functions.
## As described above, the environment of these functions works as a cache. Especially
## The operator "<< -" assigns values from the so-called "child" functions to the symbols in this cache environment.
## argument x must be a matrix object, e.g. Matrix (c (1,2,3,4), ncol = 2, nrow = 2)
## symbol x saves the martix
## symbol I save the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL 
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve():
## This function calculates the invers of the matrix, only if the cached object of earlier computations is null.
## Argument x has to be the special object returned from the "makeCacheMatrix" function.
## solve() computes the invers of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
