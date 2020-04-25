
# The goal of the makeCacheMatrix function is to create an object in form of a matrix that can cache its inverse, built as a list containing 
# another function to set the value of the matrix and of its inverse, and to get the value of the matrix and of its inverse.

# makeCacheMatrix >>> definition of the function.
# NULL >>> used for representing the null object (e.g. representing lists with zero length).
# set >>> used for creation and manipulation of sets.
# <<- operator >>> used to assign a value to an object in an environment that is different from the current environment. 
# get >>> returns the Value of a Named Object.
# list >>> represents a generic vector containing other objects. It can contain elements of different types.

# get_inver evaluates the presence of an inverse matrix stored in the object:
#       - inverse matrix stored in the object, namely "a" is not NULL, it returns "a" as the result;
#       - no inverse matrix stored in the object >>> makeCacheMatrix calls get() and get the matrix stored in the object.
#               set_inver >>> inverse matrix is calculated and stored back into the object >>> the inverse matrix is returned.

makeCacheMatrix <- function(x = matrix()) {
        
        a <- NULL
        
        set <- function(y){
                x <<- y
                a <<- NULL
        }
        
        get <- function() x
        
        set_inver <- function(inver) a <<- inver
        
        get_inver <- function() a
        
        list(set = set, get = get, set_inver = set_inver, get_inver = get_inver)
}



# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'

# use of if-else statement to return an output based on a condition. Condition is that a is not NULL. ! is used for negation of the condition.
# output message for retrieval of data in cache
# a is returned
# application of solve to the matrix, for computing the inverse of a square matrix 

cacheSolve <- function(x, ...) {
        
        a <- x$get_inver()
        if(!is.null(a)){
                message("Sto recuperando i dati nella cache")
                return(a)
        }
        data <- x$get()
        a <- solve(data)
        x$set_inver(a)
        a      
}
