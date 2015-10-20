## The following two functions are used to create a special object
## that stores a matrix and cache's its inverse ("solve()" function).


## The first function "makeCacheMatrix" creates a special object
## that is a list of following functions:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {                     ##sets the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                      ##gets the value of the matrix
        setsolve <- function(solve) m <<- solve  ##sets the value of the inverse
        getsolve <- function() m                 ##gets the value of the inverse
        list(set = set, get = get,               ##creates a list to store all four functions
             setsolve = setsolve,
             getsolve = getsolve)
}

## The second function first checks if there is already been calculated inverse
## matrix in the cache. If yes, it returns data from the cache, if no, it 
## calculates inverse matrix.

cachesolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {                        ## checks if calculated inverse matrix in the cache
                message("getting cached inverse matrix")
                return(m)                        ## Return a matrix that is the inverse of 'x' from cache
        }
        data <- x$get()
        m <- solve(data, ...)                    ##calculates inverse matrix
        x$setsolve(m)
        m                                        ## Return a matrix that is the inverse of 'x'
}
