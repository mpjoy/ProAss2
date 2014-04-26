# Here we use the `<<-` operator to assign a value to an object 
# in an environment that is different from the current 
# environment. Here are two functions, namely 'makeCacheMatrix' 
# and 'cacheSolve', are used to create a special object that 
# stores a matrix and caches its inverse.

# This function, `makeCacheMatrix` creates a special "matrix", 
# which is a list containing a function to

# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
      # This function caches the inverse of a nonsingluar matrix
      ix <- NULL

      # set the matrix
      set <- function(y) {
           x <<- y
           ix <<- NULL
      }

      # get the matrix
      get <- function() x

      # set the inverse; 'solve' is utilized to find inverse
      setinverse <- function(solve) ix <<- solve

      # get the inverse
      getinverse <- function() ix
     
      # create the list
      list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


# This function calculates the inverse of the special 'matrix' 
# created with the 'makeCacheMatrix' function. It first checks 
# to see if the inverse has already been calculated. If so, it 
# 'get's the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets 
# the value of the inverse in the cache via the 'setinverse' 
# function.


cacheSolve <- function(x, ...) {
      # Return a matrix that is the inverse of 'x'
     	
      ix <- x$getinverse()
	
      # if 'inverse' exists in the cache uses it
     	if(!is.null(ix)) {	
           message("getting cached data")
           return(ix)	
     	}
	
      # if inverse is not in the cache it calculates
     	data <- x$get()
     	ix <- solve(data, ...)
     	x$setinverse(ix)
     	ix
}
