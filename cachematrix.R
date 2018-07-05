
## The following function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	# We want to do the following:
	# 1. set the matrix
	# 2. get the matrix
	# 3. set the inverse of the matrix
	# 4. get the inverse of the matrix
	
	
	#Initially set the inverse variable ("inv") to NULL
	inv <- NULL
	
	# Set the matrix to itself
	set <- function(y) {
      	x <<- y	
		inv <<- NULL
        }	
	
	# Get the matrix itself
	get <- function() x

	# Set the inverse of the matrix	
      setinverse <- function(inverse) inv <<- inverse

	# Get the inverse of the matrix	
      getinverse <- function() inv

	# Set the functions into a list so the cacheSolve function can utilize "$" operator
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

	# Check to see if a cached inverse already exists in the makeCacheMatrix function
	inv <- x$getinverse()
	
	# If a cached inverse does exist, then inform the use that a cached inverse matrix is being retrieved and return the cached matrix
      if(!is.null(inv)) {
              message("getting cached inverse matrix")
              return(inv)
      }

	# Retrieve the matrix
      matrix <- x$get()

	# Calculate a new invese of the matrix and store the value in the variable "inv"	
      inv <- solve(matrix)

	# Cache the newly calculated inverse
      x$setinverse(inv)

	# Return inverse value	
      inv
}



