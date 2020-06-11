## The first function creates a special matrix that can be used with the second matrix  
## in order to get the inverse matrix. The data is stored in the cache.

## This function takes a matrix and returns a list that contains information about the matrix.
## @param: x invertible matrix
## @return:List with information about the matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
  invX<-NULL
  set<-function(y){
    x<<-y
    invX<<-NULL
  }
  get<-function()x
  setInv<-function(invMatrix=matrix())invX<<-invMatrix
  getInv<-function()invX
  list(set=set,get=get,getInv=getInv,setInv=setInv)
}


## This function takes a special matrix and returns its inverse
##@param: x special matrix. x is the return value of the makeCacheMatrix function
##@return: the inverse of matrix x

cacheSolve <- function(x, ...) {
  invX<-x$getInv()
  if (!is.null(invX)) {
    message("Getting cached data")
    return(invX)
  }
  invX<-solve(x$get(), ...)
  x$setInv(invX)
  invX
}
