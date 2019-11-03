## The functions below allow the creation of a matrix object cache (first function) and can be evaluated
## for its inverse (second function). If the inverse has already been generated for a particular matrix object,
## the second function will simply return the cached inverse value instead of applying the solve() function again.

## makeCacheMatrix creates a list of functions to set and get a matrix object, as well as create
## the functions to cache the inverse of the matrix and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve uses the list of functions from makeCacheMatrix() above when a square matrix has been passed to it.
## When a square matrix is passed to makeCacheMatrix() and cacheSolve is used, it first evaluates if variable m in the list 
## is NOT null and then returns m. This would be the m value in the context of the makeCacheMatrix() environment. If the value
## of m is null, then it uses the get function from makeCacheMatrix() to obtain the matrix object and apply the solve() function
## to it. Lastly, the cacheSolve() function uses the defined setinverse() function to cache the newly defined m (inverted matrix)
## object, as well as print it's value.

cacheSolve <- function(x, ...) {
  
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
