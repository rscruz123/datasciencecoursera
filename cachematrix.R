## (Write a short comment describing this function)
##### This function creates a list of 4 functions, used in conjunction with the 
##"CacheSolve" function. It sets and gets (in other words, stores and returns)
##the matrices and inverses.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


#####Cache solve checks to see if the inverse has already been stored in memory (m).
##If so, the function will terminate, let the user know that it's retrieving data,
##and return the inverse. If no inverse is found, it will recalculate the inverse
##and store it into memory. In conclusion, the solution will be returned.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
