## the program works by first defining a function to set and retreive the values of the input matrix and inverse in cache
## then another function can use these methods to set and get to either retreive cache values or replace them


# the output variable of the makeCacheMatrix function is an array of the 4 functions
# these functions will be used in the cacheSolve function
# the four functions are used to set and get the values of x (the input matrix whome inverse we need to calculate)
# and to set and get the value of the inverse if x.
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  
  # set function is used to set the value of x to the value of the new matrix. 
  # It also resets the value of m (which representes inverse) to NULL to ensure we dont retreive the old values.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # the get function is used to retreive the value of x.
  get <- function() x
  
  # the setinv function is used to store the value of calculated inverse in the variable m
  setinv <- function(inv) m <<- inv
  
  # the getinv function is used to retreive the value of inverse from cache
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# this function will detect if the inverse of the matrix exist. If it does exist,it will retreive it
# if the inverse does not exist it will calculate it and then cache it
cacheSolve <- function(x, ...) {
  
  # the first step is to find the value of inverse in the cache.
  # if the value is cache is not NULL, then it is ok to use it 
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if the value of inverse in cahce was Null, we need to retreive the matrix and calcualte its inverse.
  data <- x$get()
  m <- solve(data, ...)
  
  # once the new inverse is calculated we store it in cache
  x$setinv(m)
  m
}