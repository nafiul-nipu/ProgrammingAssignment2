## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creates a cache matrix from a given matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # initialize the cache Matrix
  cache_Matrix <- NULL
  
  # lets define the setmatrix method
  setMatrix <- function(y) {
    x <<- y
    cache_Matrix <<- NULL
  }
  
  # now lets define the getMatrix method
  getMatrix <- function() x
  
  # define setCache
  setCache <- function(inverse) cache_Matrix <<- inverse
  
  #define getCache
  getCache <- function() cache_Matrix
  
  # list the names
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)

}


## Write a short comment describing this function
#return the inverse of a given matrix utilizing the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #check the values of cache_matrix
  cache_Matrix <- x$getCache()
  
  # if not null -  return the result 
  if (!is.null(cache_Matrix)) {
    message("getting cached data")
    return(cache_Matrix)
  }else {
    # if empty then get the matrix
    # create, set, update and return the cache matrix
    data <- x$getMatrix()
    cache_Matrix <- solve(data, ...)
    x$setCache(cache_Matrix)
    return(cache_Matrix)
  }
}
