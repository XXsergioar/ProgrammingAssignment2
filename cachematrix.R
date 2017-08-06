## These are two functions, one to input a matrix from the cache its matrix
## and another to compute the inverse of the matrix if it doesn't exist
##
## The first function creates a list, with the matrix input from the cahe 
## and its inverse
## if the inverse already exist, if not, the inverse is NULL. 
## The output of the funcion is a list with four elements
##
makeCacheMatrix <- function(x = matrix()){ ##creates a "matrix" that can cache its inverse
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
## This second function computes the inverse of the matrix, if its couldn't 
## be retieved from the cache in the previous function. The output is INV
##
cacheSolve <- function(x, ...) { ##computes, or retrieves, inverse of "matrix
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
