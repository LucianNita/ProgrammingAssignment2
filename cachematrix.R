## [Put comments here that describe what your functions do]

makeCacheMatrix <- function(x = matrix()) {
  
  ## This function works like a class, it creates a list 
  ## with 4 member functions: set, get, setInv and getInv. 
  ## It uses <<- assignment operator so that these local 
  ## variables are not visible to the outside environment. 
  
  xinv <- NULL 
  set <- function(y) {
    x <<- y
    xinv <<- NULL 
  }
  
  get <- function() x   
  setInv <- function(inv) xinv <<- inv 
  getInv <- function() xinv 
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## m is assigned the value of an inversed matrix from object 'x'.
  ## If m is NOT NULL, then then the inversion result already exists.
  ## In that case it is retuned and used. If m IS NULL we calculate the
  ## inverse and we return it.
  m <- x$getInv() 
  if(!is.null(m)) { 
    message("getting cached data")
    return(m) 
  }
  data <- x$get() 
  m <- solve(data,...)
  x$setInv(m) 
  m
}
