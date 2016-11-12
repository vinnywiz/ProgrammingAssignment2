## Put comments here that give an overall description of what your
## functions do
## The purpose of this program is to get the inverse of a matrix
## The program also caches the inverse of the matrix so it can be retrieved in an efficient manner
## without solving for the inverse multiple times. 

## Write a short comment describing this function
## The first function creates a matrix that can store the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){                                             ## sets the value of a matrix
                x<<- y
                inverse <<- NULL
        }
        get <- function()x                                              ## gets the value of a matrix
        setInverse <- function(inversing) inverse <<- inversing          ## sets the inverse of a matrix
        getInverse <- function() inverse                                ## gets the inverse of the matrix
        list(set =set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## The function is used to solve the inverse of a matrix and retrieves a stored inverse of the matrix
## when the inverse was solved on a prior instance. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)){                                          ## retrieves the cached matrix
                message("grabbing the cached matrix")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)                                      ## solves the uncached matrix 
        x$setInverse(inverse)                                           ## calls the function to cache the inverse of the matrix
        inverse
}


########################################################################
## Values used to test the above functions

## uncomment to test the below sample test data

# matrix <- matrix(1:4, 2,2)
# cachematrix <- makeCacheMatrix(matrix)
# cacheSolve(cachematrix)

## running the last line again instead of solving for the inverse of the matrix again 
## retrieves the the cached inverse matrix
# cacheSolve(cachematrix)

