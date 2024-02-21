## 'makeCacheMatrix' function will create a special matrix that is a list of functions that will cache the value of the inverse matrix
## 'cachesolve' function will either calculated for the first time the inverse matrix and cache the value or retrieve the
##  cache data if the inverse is already calculated


## This first function create a matrix (which one assumed is a square and invertible)
## and build a set of functions stored as a list in the makeCacheMatrix environment.
## The inverse matrix is not yet calculated in this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## 'inv' object is initialized and set as 'NULL' in the 'makeCacheMatrix' environment
    set <- function(y) { ## set the value of a new matrix 'y'
      x <<- y ## assign y to x in the parent environment i.e. 'makeCacheMatrix' environment rather than the 'set' function
      inv <<- NULL ## clear values of 'inv' that have been cached by prior execution of 'cacheSolve'
    }
    get <- function() x ## will retrieve the correct value (matrix) for 'x', 
    setinverse <- function(solve) inv <<- solve 
    getinverse <- function() inv ## will retrieve the correct value (inverse matrix) for 'inv'
    
    list(set = set, get = get,  ## generate a list of the four functions defined above so they could be called with the symbol '$'         
         setinverse = setinverse,
         getinverse = getinverse)
}


## This second function return a matrix inverse of 'x'. If the inverse matrix has not been calculated
## 'inv' is null and the value of the inverse matrix will be stored in 'inv' and returned.
## If the inverse matrix was previously calculated, 'inv' is not null anymore and the stored value or cache
## will be returned along with the message 'getting cached data'.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() ## set the value of the solve function in 'inv'
  if(!is.null(inv)) { ## check whether the inverse matrix was previously calculated and stored in 'inv'
    message("getting cached data")
    return(inv) ## return the cached inverse matrix along with the text"getting cached data" for previously calculated inverse matrix
  }
  data.m <- x$get() ## if 'inv' was null, the new matrix is stored in 'data.m'
  inv <- solve(data.m,...) # then the inverse matrix is calculated for the first time
  x$setinverse(inv) # set the inverse of the matrix in the input object 'inv'
  inv ## return the calculated inverse matrix stored in 'inv'
}

