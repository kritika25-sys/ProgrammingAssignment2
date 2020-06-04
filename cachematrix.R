## This is the Peer-graded Assignment: Programming Assignment 3 on Lexical Scoping
## This assignment has two functions (1) makeCachematrix (2)cacheSolve

## makeCacheMatrix() function creates a special matrix object that can cache its inverse.
## This function takes in a matrix A as an argument and operates 4 functions set(), get(),
## setinv() and getinv() on it. 
## (1) set() - takes in another matrix B of same dimensions and assigns its values to A
## (2) get() - prints the matrix A
## (3) setinv() - takes in another matrix  of same dimensions and assigns it as inverse of A
## (4) getinv() - prints the inv matrix obtained using setinv() function.

makeCacheMatrix <- function(x = matrix()) {
  res<-NULL
  set<-function(y){
    x<<-y
  res<<-NULL
  }
  get<-function() x
  setinv<- function(inv) res<<-inv
  getinv<-function() res
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  }


## cacheSolve() function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed),then the cachesolve retrieves the inverse from the cache along with
## printing the message "getting cached data". 

cacheSolve <- function(x, ...) {
  res<-x$getinv()
  if(!is.null(res)){
    message("getting cached data")
    return(res)
  }
data<-x$get()
  res<-solve(data)
  x$setinv(res)
  res
}
