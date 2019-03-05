## Cache a matrix as an r object with a cache indicator, cache variable, and 4 operators
## The 2 variables and 4 operator functions are made available to the parent environment
## through lexical scoping

##Usage: initialize a matrix cache via calling makeCacheMatrix
##e.g. : matc <-- makeCacheMatrix()
##assign a matrix to the matrix cache via teh set function
##e.g. : > p <-- matrix(sample.int(100,9), nrow=3,ncol=3)
##> matc$set(p)
##call cacheSolve to solve the stored matrix
##e.g. : cacheSolve(matc)
##on the first solve, since the cahce flag is NULL, 
##the solution will run and be stored in x within the makeCacheMatrix object
##on further runs of cacheSolve(), assuming setsolve has not been called to change x, 
##the cached solution will be returned instead 



## initialize an empty matrix cache object (passed argument assigned to x), and set cache flag (m) as false
## makes available operator functions on the object

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m<<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## solves (matrix inverse) the matrix stored in the makeCahceMatrix object
## checks the flag m - if not NULL, return the cahced solution instead

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
