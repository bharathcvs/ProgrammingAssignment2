## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix as an argument and creates a special list
## This list can be passed to the cacheSolve function to get the inverse of the
## original matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  get<- function() x
  set<- function(y) {
    x<<-y
    inv<<-NULL
  }
  getinv<-function() inv
  setinv<-function(i) { inv<<-i}
  list(get=get,set=set,setinv=setinv,getinv=getinv)
}


## This function takes the special list that makeCacheMatrix
## and returns the inverse of the original matrix that was passed to makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
    message ("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setinv(inv)
  inv
}
