## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function makeCacheMatrix has four functions:
## 1. setmatrix, sets/caches the value of the matrix
## 2. getmatrix, gets the value of the matrix, if it already exists
## 3. setinv, sets/caches the value of the inv
## 4. getin, gets the value of the pre-calculated inverse
makeCacheMatrix <- function(x = matrix())
{	
	m <- NULL
	setmatrix <- function(y = matrix())
	{
		x <<- y
		m <<- NULL
	}
	getmatrix <- function() x
	setinv <- function(inv) m <<- inv
	getinv <- function() m
	list(setmatrix = setmatrix, getmatrix = getmatrix, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## The function cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse is pre-calculated then retrieves it from the cache.
cacheSolve <- function(x, ...)
{
	## Return a matrix that is the inverse of 'x'
	m <- x$getinv()
	if(!is.null(m))
	{
		message("getting cached data")
		return(m)
	}
	data <- x$getmatrix()
	m <- solve(data)
	x$setmatrix(m)
	m
}
