## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mateff <- NULL
  set <- function(y){
    x <<- y
    mateff <<- NULL
  }
  get <- function() x
  setinv <- function(matinv) mateff <<- matinv
  getinv <- function() mateff
  list(set = set,get = get,setinv = setinv,getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  meff <- x$getinv()
  if(!is.null(meff)){
    message("getting cached data")
    return(meff)
  }
  data <- x$get()
  meffcal <- solve(data,...)
  x$setinv(meffcal)
  meffcal
}
