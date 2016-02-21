## Put comments here that give an overall description of what your
## functions do

## Creates matrix that first sets the value of the matrix, get the value of the matrix, 
## sets the the inverse of the matrix, gets the inverse of the matrix.

makeCacheMatrix <- function(x=matrix()){  
  inv <- NULL                             
  set <- function(y){                     
    x<<-y                                 
    inv <<-NULL                           
  }
  get <-function() x                      
  set_inv <- function(inverse) inv <<- inverse 
  get_inv <- function() inv                     
  list(set=set, get=get, set_inv=set_inv, get_inv=get_inv) 
}

## Takes the inverse of the matrix from above function, returns the cached matrix if the matrix hasn't changed.

cacheSolve <- function(x,...){
  inv <- x$get_inv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$set_inv(inv)
  inv
}
