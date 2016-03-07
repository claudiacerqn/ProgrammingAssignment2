## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        ## based on code shared @ masterr.org
        ## the created matrix x is a square invertible one
        ## this block returns a list containing the follwing functions: set & get the matrix; set and get the inverse
        ## this list is used as the input to cacheSolve()
        inv = NULL
        set = function(y) {
                # `<<-` is used to assign a value to an object in an environment 
                # different from the current environment
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
        ## x is the output of makeCacheMatrix()
        ## it returns the inverse of the original matrix input to makeCacheMatrix()
        ## according to the assignment instructions, in case the inverse has already been calculated, 
        ## then the cachesolve should retrieve the inverse from the cache
        
        inv = x$getinv()
        # get and calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}
