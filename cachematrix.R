###########################################################
## These are the functions for Programming Assignment #2  #
## in the course "R Programming"                          #
###########################################################

## Create a matrix object that can cache it's inverse
## the function has four sub-functions
makeCacheMatrix <- function(x = matrix()) {

  ## establish the variable m and empty it
  m <- NULL                                      
  
  ########################################################
  ## SET (our first sub function)                        #
  ########################################################
  set <- function(y) {
    x <<- y              ## set x globally to the passed value y
    m <<- NULL           ## set m globally to NULL
  }
  
  ####################################################
  ## GET (our second sub function)                   #
  ####################################################
  get <- function() x
  
  ####################################################
  ## SETINVERSE (our next sub function)             #
  ####################################################
  setinverse <- function(solve) m <<- solve
  
  ####################################################
  ## GETINVERSE (our next sub function)             #
  ####################################################
  getinverse <- function() m
  
  ## store these functions in the list, so that we have all our "sub" functions
  ## in case someone were to list() all the subfunctions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Compute the inverse of a "matrix"
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## by setting m to the result of the "sub" function getinverse 
  m <- x$getinverse()
  
  if(!is.null(m)) {                     ## was a cached value set?
    message("getting cached data")      ## if so, then let's tell them so
    return(m)                           ## and return the cached value
  }
  
  ## if we made it this far, then a cached value
  ## was not set and we need to invert the matrix
  
  data <- x$get()                      ## call get function and put results into var data
  m <- solve(data, ...)                ## invert data and put results into var m
  x$setinverse(m)                      ## call setinverse function and pass it m
  m                                    ## this will display the contents of m
}

