## An object oriented way to cache an inverse of a matrix

# Note: the input matrix must be invertible
# An example: 
# > a<-makeCacheMatrix(matrix(c(4,7,2,6),2,2))
# > cacheSolve(a)
#      [,1] [,2]
# [1,]  0.6 -0.2
# [2,] -0.7  0.4
# > cacheSolve(a)
# getting cached data
#      [,1] [,2]
# [1,]  0.6 -0.2
# [2,] -0.7  0.4


# Desc: Create an object with 4 methods, which is associated with a matrix
# Parameter: a matrix
# Return: a list containing set/get/setinverse/getinverse methods
makeCacheMatrix <- function(x = matrix()) {
 	m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Desc: Calculate and cache the inverse of the object if it is not cached
# else return the cache directly 
# Parameter: The return value of makeCacheMatrix containing a list of 4 methods
# Return: The inverse of the matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

