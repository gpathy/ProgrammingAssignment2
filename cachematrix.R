## The two functions here make use of environments to cache results that 
## are expensive to compute and are used many times. 
## Here the inversion of a matrix is acheived using caching. 
## note that the matrix is considered invertible.


## function makeCacheMatrix, takes a regular matrix
## and creates a special type of matrix, that includes setters and getters
## and a cache variable that stores the inverse 
makeCacheMatrix <- function(x = matrix()) {
minv <- NULL
set <- function(y){
  x <<- y
  m <<- NULL
}
get <- function() x
setinv <- function(solve) minv <<-solve
getinv <- function() minv
list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## function cacheSolve checks if the inverse for this matrix is already 
## available in the cache, if so, it returns the cached inverse
## if its not available it computes the inverse, stores it in the cache
## and then returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setinv(minv)
  minv
  
}
