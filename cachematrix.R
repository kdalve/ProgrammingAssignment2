## The function, makeCacheMatrix, creates a function that can store its calculated inverse. The inversed matrix is stored outside of the function using the <<-operator. 
## The cacheSolve function computes the inverse of the matrix, if the inversed matrix was already stored using the makeCacheMatrix function, then it will not have to be computed again and will be retrieved by the function. 
##Reducing computation time.

#makeCacheMatrix establishes the function and sets the arguments.Stores the inversed matrix in the parent environment.
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s<<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve solves the inverse of the matrix, but if the inverse of the matrix has already been computed it will return the matrix, saving computation time.
cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}