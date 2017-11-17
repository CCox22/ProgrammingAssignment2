## The following two functions enable the inverse of a matrix to be cached.

## The makeCacheMatrix function creates a "special" matrix which is a list
## containg 4 functions: 1) sets the value of the matrix; 2) gets the value
## of the matrix; 3) sets the value of the inverse matrix; 4) gets the value
## of the inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inverse <<- inverse
	getinverse <- function() inverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## The cacheSolve function returns the inverse of the square matrix, 'x'.
## It checks whether the inverse has already been computed and cached. 
## If it has already been computed and cached, cacheSolve prints the message
## "getting cached data" and returns the inverse from the cache without having
## to compute it.
## If it has not been computed and cached previously, it computes it using
## the solve() function and sets the inverse in the cache (i.e. sets the
## inverse in the "special" matrix).
 

cacheSolve <- function(x, ...) {
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	} else {
		data <- x$get()
		inverse <- solve(data)
		x$setinverse(inverse)
		inverse
	}
}



