## Computing the inverse of a square matrix: Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.

## This function creates a special "square matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	xInverse <- NULL
	set <- function(y) {
		x <<- y
		xInverse <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) xInverse <<- inverse
	getInverse <- function() xInverse
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special "square matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xInverse <- x$getInverse()
        if (!is.null(xInverse)) {
        	message("getting cached data")
        	return(xInverse)        	
        }
        data <- x$get()
        xInverse <- solve(data, ...)
        x$setInverse(xInverse)
        xInverse
}



## code testing
#myMatrix <- makeCacheMatrix(x=matrix(1:4, nrow=2))
#myMatrix
#cacheSolve(myMatrix)