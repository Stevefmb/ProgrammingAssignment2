## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y 
##      "The operators <<- and ->> are normally only used in functions, and
##      cause a search to be made through parent environments for an existing
##      definition of the variable being assigned. If such a variable is found
##      (and its binding is not locked) then its value is redefined, otherwise
##      assignment takes place in the global environment." R-help          
                inv <<- NULL
                
        }
        get <- function() x
        setInverse <- function(inverse) <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## This function computes the inverse of the special matrix created by
## the makeCache function created above. If the inverse already exists it will 
## use the cached invers. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
## '$' is used to indicate a particular variable in a data frame
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        
}
