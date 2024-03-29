makeCacheMatrix <- function(x = matrix()) {
	# initialises makeCacheMatrix m to NULL
	m <- NULL
	set <- function(y) {
	# y is by default a matrix
	# replace makeCacheMatrix x by set y
	# replace makeCacheMatrix m by NULL
		x <<- y
		m <<- NULL
	}
	get <- function() {
		x
	}
	setinverse <- function(inverse) {
		m <<- inverse
	}
	getinverse <- function() {
		m
	}
	list(set = set, get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}