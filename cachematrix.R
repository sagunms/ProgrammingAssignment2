
## = Creates a special matrix object for caching the inverse of matrix == ##

makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse matrix
  inv <- NULL
  
  # setter: to set matrix and make inverse matrix null
  # <<- operator which can be used to assign a value to an object 
  # in an environment that is different from the current environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # getter: just to get matrix
  get <- function() {
    x
  }
  
  # setter: to set the inverse matrix via cacheSolve
  # <<- operator which can be used to assign a value to an object 
  # in an environment that is different from the current environment
  setinv <- function(inverse) {
    inv <<- inverse
  }
  
  # getter: to get the inverse matrix via cacheSolve
  getinv <- function() {
    inv
  }
  
  # return special matrix object which is actually a list
  # containing a function to set+get matrix and set+get inverse matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)    
}

## = Calculates the inverse of the special matrix object == ##
## It first checks if inverse has previously been computed.
## If so, it retrieves the inverse matrix from the cache.
## Else, it computes the inverse matrix and store in cache.
## "solve" function is used to calculate inverse matrix

cacheSolve <- function(x, ...) {
  # try to get inverse from cache
  inv <- x$getinv()
  
  # if matrix already cached, simply return it
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  
  # else, compute inverse and cache it for future
  matr <- x$get()
  inv <- solve(matr)
  x$setinv(inv)
  
  # return inverse matrix
  inv
}

## Example run:
# mtx <- makeCacheMatrix(matrix(c(2,4,6,8), 2, 2))
# cacheSolve(mtx)
# 
#        [,1]  [,2]
# 	[1,] -1.0  0.75
# 	[2,]  0.5 -0.25
# 
# cacheSolve(mtx)  
# 
# # Output:
# 	getting cached inverse matrix
# 	     [,1]  [,2]
# 	[1,] -1.0  0.75
# 	[2,]  0.5 -0.25
# 
# mtx$set(matrix(10:13, 2, 2))
# cacheSolve(mtx)
# 
# 	     [,1] [,2]
# 	[1,] -6.5    6
# 	[2,]  5.5   -5
# 
# cacheSolve(mtx)  
# 
# # Output:
# 	getting cached inverse matrix
# 	     [,1] [,2]
# 	[1,] -6.5    6
# 	[2,]  5.5   -5# 
