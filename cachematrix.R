##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){          #set the value of the matrix
    x<<-y
    m<<-NULL
  }
  get<-function() x         #get the value of the matrix
  setInverse<-function(solve) m<<-solve    #set the value of the inverse  
  getInverse<-function() m                 #get the value of the inverse
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##above. If the inverse has already been calculated, 
##then the cachesolve should retrieve the inverse from the cache.

## Computing the inverse of a square matrix can be done with the solve function in R. 


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()        #query the x matrix's cache
  if(!is.null(m)){         # if the cache already exists
    message("getting cached data")
    return(m)              #return cache value
  }
  data<-x$get()            # else
  m<-solve(data, ...)      # compute the inverse
  x$setInverse(m)          # save the result to the cache
  m                        # return the inverted matrix
}
