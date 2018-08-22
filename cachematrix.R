## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## input parameter: numeric matrix
## accessor: get() - returns the original matrix, eg. cacheMatrix$get()
## accessor: getsolve()- returns the inverse. Default value is NULL. 
## setter: set() - parameter: matrix. Resets the matrix to the new matrix provided and inverse value i to NULL
## setter: setsolve() - parameter: matrix. Sets the inverse value of the original matrix

## Description:
## this function is a storage (cache) for a square matrix and its inverse. 


makeCacheMatrix <- function(x = matrix(data = NA, nrow = 2, ncol = 2)) {
  i<-NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
    }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  
  list(set = set,
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  

}


## Write a short comment describing this function
## Input: a cached object that is created by the above makeCacheMatrix function. It does not take a matrix as a paramter
## Return: inverse of the matrix that is cached in the makeCacheMatrix.
## description: this function retrives the inverse of the cached matrix. If the inverse does not exist in th cache,
## it calculates the inverse, stores it in the cache and returns the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
