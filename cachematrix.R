## makeCacheMatrix function creates a list of the functions needed to ## be fed into cashSolve which caches solutions and returns them if ##they exist in the cache otherwise computes it 


## makeCacheMatrix creates a vector containing following functions 1) set the value of the matrix, 2) get the value of the matrix, 3) set the value of the mean, 4) get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve calculates the inverse of the matrix, if it exists in cache it returns it from memory

cacheSolve <- function(x, ...) {
	 m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
