## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function returns a "special" matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	
	list(set = set,
		 get = get,
		 setinverse = setinverse,
		 getinverse = getinverse
	)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	# get the matrix from x
	mat <- x$get()
	
	# find its inverse
	inverse_mat <- solve(mat, ...)
	
	# converting the inverse into a "special" matrix using the above function
	inverse <- makeCacheMatrix(inverse_mat)
	
	# inverse(inverse(x)) = x
	inverse$setinverse(x)
	
	# caching inverse(x)
	x$setinverse(inverse)
	
	#return inverse
	inverse
}
