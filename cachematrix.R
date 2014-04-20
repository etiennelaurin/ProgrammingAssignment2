## Functions that create a matrix capable of caching its inverse and
## the necessary function to access and create that cache version
## of the inverse

## makeCacheMatrix creates a matrix capable of caching its inverse

makeCacheMatrix <- function(x = matrix()) {
	# The variable inv contains the inverse of the matrix
	inv <- NULL
	# The function set assigns a matrix to the cache matrix
    set <- function(y) {
		x <<- y
        inv <<- NULL
    }
	# The function get returns the matrix
    get <- function() x
	# The function setinverse assigns the inverse of the matrix
	# to the cache
    setinverse <- function(solve) inv <<- solve
	# The function getinverse returns the inverse of the matrix
    getinverse <- function() inv
	# Return the group of functions
    list(set = set, get = get,
		setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix, from cache if
## possible and if not calculates it and saves it to cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		# Get the current cached inverse of the matrix
		inv <- x$getinverse()
		# If there is a cached inverse
        if(!is.null(inv)) {
			# Return the current cached inverse
            message("getting cached data")
            return(inv)
        }
		# If not, get the matrix data
        data <- x$get()
		# Calculate the inverse of the matrix
        inv <- solve(data, ...)
		# Save the inverse of the matrix to cache
        x$setinverse(inv)
		# Return the inverse of the matrix
        inv
}
