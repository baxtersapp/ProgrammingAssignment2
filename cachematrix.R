##This caches a matrix. This will be used below. One can set the matrix to any type that they want.
##Defined within are the rules of set, get, setinverse and getinverse, which allow a sort of 
##memory to the program. Very cool project, had to look for some help online but managed to get it
##working mostly on my own! Hope it went well for you, too.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}



##The function below solves the inverse of a matrix. It then sets the inverse in the cache as per
##the rules defined above - specifically, setinverse - so it can be recalled later by using
##the function getinverse in conjuction with the name of your matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
