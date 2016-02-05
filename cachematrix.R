

#Function "makeCacheMatrix" contains several functions.
#-it begins by setting the In to NULL as a placeholder for a future value.
#-it defines a function to set the matrix, x, to a new matrix, y, and resets the inverse, In, to NULL.
#-get is a function that returns the matrix x stored in the main function.
#-set is a function that changes the matrix stored in the main function.
#-setinverse store the value of the input in a variable In .
#-getinverse returns it.
#-returns the 'special matrix' containing all of the functions just defined.

makeCacheMatrix <- function(x = matrix()) {
  In <- NULL
  set <- function(y) {
    x <<- y
    In <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) In <<- solve
  getinverse <- function() In
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




#Function "cacheSolve" returns the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated, then the cacheSolve should get the result from the cache. 
#If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, calculates the inverse, and x$setinverse(In) stores it in the object In in makeCacheMatrix.



cacheSolve <- function(x, ...) {
  In <- x$getinverse()
  if(!is.null(In)) {
    message("getting cached data")
    return(In)
  }
  data <- x$get()
  In <- solve(data, ...)
  x$setinverse(In)
  In
}