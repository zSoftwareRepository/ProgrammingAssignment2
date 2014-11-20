
## Function makeCacheMatrix creates a matrix
## object that precalculates the inverse of a square matrix
makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize m with NULL value for later validation
  m <- NULL
  
  # Function "set" assign new matrix value to the cached matrix object
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Function "get" point an objet to x (assign x to an object)
  get <- function() x
  
  # Function "setsolve" point m to the value of the solve parameter
  setsolve <- function(solve) m <<- solve
  
  # Function "getsolve" make an object point m to the value 
  getsolve <- function() m 

  # list to hold all functions for the cached matrix object
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Calculates the inverse of a square matrix if not already 
## Cached
cacheSolve <- function(x, ...) {
  
  # point m to the value returned by the function getsolve in the 
  # object x (parameter)
  
  m <- x$getsolve()
  
  # If m is not null retrieve cached object m
  # andexit the function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Point data to the vlaue returned by X$get() function
  data <- x$get()
  
  # Calculate the inverse of the matrix pointed by data
  m <- solve(data)
  
  # cache the caculated inverse in the object x (m object)
  x$setsolve(m)
  m
} 