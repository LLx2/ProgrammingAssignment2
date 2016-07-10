## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()){
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        seti <- function(inverse) i <<- inverse
        geti <- function() i
        list(set = set, get = get,
             seti = seti,
             geti = geti)
}

#Return a matrix that is the inverse of x
cacheSolve <- function(x, ...){
        i <- x$geti()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mtrx <- x$get()
        i <- solve(mtrx, ...)
        x$seti(i)
        i
}
