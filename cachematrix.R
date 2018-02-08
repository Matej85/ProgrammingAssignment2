#Double function using Lexical scoping concept to calculate a matrix inversion

## makeCacheMatrix function - to store objects/variables, 
##inputs as square matrices only

## cacheSolve function - to calculate the inversion of the matrix

makeCacheMatrix <- function(x = matrix()) {
        sol <- NULL
        set <- function(y) {
                x <<- y
                sol <<- NULL
        }
        get <- function() x
        setSolv <- function(solv) sol <<- solv
        getSolv <- function() sol
        list(set = set, get = get,
             setSolv = setSolv,
             getSolv = getSolv)
}


cacheSolve <- function(x, ...) {
        sol <- x$getSolv()
        if(!is.null(sol)) {
                message("getting cached data for matrix inversion")
                return(sol)
        }
        data <- x$get()
        sol <- solve(data, ...)
        x$setSolv(sol)
        sol
}
