## Encapsulate matrix primitives and establish an efficiency
## on storing a cached version of the inverse of the stored
## matrix when something asks for the inverse.  The inverse
## is calculated once and stored, not calculated each time
## the inverse is asked for.

## function makeCacheMatrix
##     A constructor function to instantiate an encapsulation
##     of a matrix primitive.  Provides get() and set()
##     methods to return the stored primitive or change the
##     primitive stored in the instance.  
##     Also provides storage for the inverse of the matrix
##     stored within.  Upon instantiation, the internal
##     inverse variable is set to NULL, but can be set
##     by passing a matrix primitive to the setinv() method.
##     getinv(), of course, returns the internal value of the
##     internal variable xinv.
##     Personal commentary:
##     Allowing a publicly-accessible setinv() method that
##     takes any matrix primitive as a parameter and stores
##     it in the internal xinv variable is a poor 
##     implementation.  setinv() should take no arguments
##     and, if internal variable x is set, calculate the 
##     inverse and store it in xinv.  Similarly, getinv()
##     should always return the value of xinv so long as
##     internal variable x is set and, if x is un-set, 
##     xet xinv to NULL and return NULL.

## function cacheSolve
##     A combination modifier function and external
##     solve method/get function for the object type 
##     created by constructor makeCacheMatrix that accepts
##     an object instance of the type created by 
##     makeCacheMatrix().
##     If the internal variable xinv is set, just return
##     the xinv matrix primitive which SHOULD represent
##     the inverse of internal matrix primitive x.
##     Else, xinv is unset and needs to be calculated.
##     Calculate the inverse, store it in xinv, and 
##     return that new matrix primitive.


## define makeCacheMatrix constructor function; see file
## header, above for detailed information about how this
## object type's internal storage and methods work.
makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        setinv <- function(p_xinv) xinv <<- p_xinv
        getinv <- function() xinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## define cacheSolve, an external modifier method and 
## 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getinv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$get()
        xinv <- solve(data, ...)
        x$setinv(xinv)
        xinv
}
