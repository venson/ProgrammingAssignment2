## Put comments here that give an overall description of what your
## functions do
# 1.set the matric
# 2.get the cached matrix
# 3.set the inverse of the cached matrix
# 4.get the inverse of the cached matrix

## Write a short comment describing this function

# create a list caching the matrix for Solve
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(
                set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve
        )
}


## Write a short comment describing this function

# solve the matrix which is cached in a list
cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        if (!is.null(inv)) {
                message("getting cached matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv ## Return a matrix that is the inverse of 'x'
}
