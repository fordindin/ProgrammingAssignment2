## Matrix inversion quite expensive operation, so it would be better
## to cache already inverted matrices. Here is the set of functions to
## to so.

## makeCacheMatrix is wrapper for matrix() function. It allows to store
## inverse of matrix in addition to matrix itself.
makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
				x <<- y
				m <<- NULL
		}
		get <- function() x
		setinverse <- function(i) inv <<- i
		getinverse <- function() inv
		list(
				set = set,
				get = get,
				setinverse = setinverse,
				getinverse = getinverse
				)
}


## cahceSolve is function, based on solve() function, which is allows to cache
## inversion of matrix to avoid inversion of same matrix multiple times

cacheSolve <- function(x, ...) {
		m <- x$getinverse()
		# check for cached value, return it if exists
		if (!is.null(m)) {
			message("Getting inverse from cache")
			return(m)
		}
		solution <- solve(x$get(), ...)
		## cache solution
		x$setinverse(solution)
		## Return a matrix that is the inverse of 'x'
		solution
}
