## Below are two functions that, when used together, allow the user to save ("cache") 
## previous iterations of a particular function (in this case solve(), which provides
## the inverse of a matrix). Saving the previous iterations will allow the user to save
## processing time by not repeating the solve() function more than once on the same input matrix.
## This would be very useful if I had a large list of matrixes I wanted to get the inverse of (and 
## some of them may repeat) and also if the input matrix updates itself frequently (and may repeat)
## and I want the inverse frequently.

## The first function will create a cache for your matrix.

makeCacheMatrix <- function(x = matrix()) {
            s <- NULL
            set <- function(y){
                  x <<- y
                  S <<- NULL
            }
            get <- function() x
            setinverse <- function(solve) s <<- solve
            getinverse <- function() s
            list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function will take the output of the first function, the matrix with cache, and return the inverse. 
## If the inverse of the matrix has been passed through cacheSolve previously, then it will pull up the cached 
## inverse of the matrix.

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}


