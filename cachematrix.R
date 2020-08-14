## This function creates a special "matrix" object
## that can cache its inverse.

##makeCacheMatrix is a function which creates a special "matrix" object that can
##cache its inverse for the input (which is we will assume its an invertible square matrix)

 makeCacheMatrix <- function(x = matrix()) {
       inver<- NULL
       set<- function(y){
         x<<-y
         inver<<- NULL  
       }
       get<- function()x
       set_inver<-function(inverse)inver <<- inverse
       get_inver<-function()inver
       list(set = set, get = get, set_inver = set_inver, get_inver = get_inver)
       
}


## cacheSolve is a function which computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve will retrieve the
## inverse from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$get_inver()
  if(!is.null(inver)) {
    message("Getting cached result")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$set_inver(inver)
  inver
}
