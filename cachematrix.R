## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function


# program 2
## this function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  
  minv <- NULL                              # the result is stored in this variable
  set <- function(y) {
    m <<- y
    minv <<- NULL                           # set xinv to null
  }
  
  get <- function() m                       # to obtain teh input matrix
  setInv <- function(inv) minv <<- inv      # set the inv. matrix
  getInv <- function() minv                 # to obtain the inv. matrix

  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## this function calculate the inverse of the special "matrix" created previously. 

cacheSolve <- function(m, ...) {
  minv <- m$getInv()                           # get the inv. matrix 

  if(!is.null(minv)) { 
    message("getting cached data")
    return(minv)                               # return the calculated inv. matrix
  }
  dato <- m$get()                           # if not, 
  minv <- solve(dato)                       
  m$setInv(minv)                               # set it to the object
  minv                                         # return the result
}
