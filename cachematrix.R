###############################################################################

## Task       : Matrix inversion is usually a costly computation and there may 
##              be some benefit to caching the inverse of a matrix rather than 
##              compute it repeatedly. 
##              This assignment is to write a pair of functions that cache the 
##              inverse of a matrix.
## Author     : Mohd A. Zainol

###############################################################################

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## A pair of functions that cache the inverse of a matrix

## Function: This function creates a special "matrix" object that can cache its 
##           inverse.

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

makeCacheMatrix <- function(m = matrix()) {

    i <- NULL				## Initialize the inverse property

    set <- function(matrix) {		## Set the matrix
		m <<- matrix
		i <<- NULL
    }

    get <- function() {			## Get the matrix
		m			## Return the matrix
    }

    setInverse <- function(inverse) {	## Set the inverse of the matrix
		i <<- inverse
    }

    getInverse <- function() {		## Get the inverse of the matrix
		i			## Return the inverse property
    }

    list(set = set, get = get,		## Return a list of the methods
         setInverse = setInverse,
         getInverse = getInverse)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## This function compute the inverse of the special matrix returned by 
## "makeCacheMatrix" above. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the "cachesolve" should retrieve the inverse from the cache.

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cacheSolve <- function(x, ...) {

    m <- x$getInverse()			## Return matrix, the inverse of 'x'

    if(!is.null(m)) {			## Return the inverse if already set
	message("getting cached data")
	return(m)
    }

    data <- x$get()			## Get the matrix from our object

    m <- solve(data) %*% data		## Calculate the inverse using:
					## matrix multiplication

    x$setInverse(m)			## Set the inverse to the object

    m					## Return the matrix:
					## the inverse of 'x'
}
