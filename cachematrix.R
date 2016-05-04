## Calculate Matrix Inversion
## Only calculate when inversion not exist
## Otherwise pick up from cache

## cache the value

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inversion) inv <<- inversion
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## calculate or pick up from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
