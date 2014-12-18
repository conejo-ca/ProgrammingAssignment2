## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to 

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## Write a short comment describing this function

# The following function calculates the inverse of the special "matrix" 
# created with the above function. However, it first checks to see if 
# the inverse has already been calculated. If so, it gets the inverse from 
# the cache and skips the computation. Otherwise, it calculates the 
# inverse of the data and sets the value of the inverse in the cache via 
# the setinverse function.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    
    # check matrix is non-singular.  stop and print an error message if it is.
    if(det(data,...) == 0)
        stop("Matrix is singular")
    
    # find and save the inverse
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
