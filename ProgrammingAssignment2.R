## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
	m <- solve(data, ...)
	x$setmatrix(m)
	m
}
