##These functions find and cache the inverse of the inputed matrix.
##If the cacheSolve finds no cached value of the inverse of the matrix
##x then it assigns one to x for future calculations.

## This function calculates the inverse of x and caches it

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set<-function(y){
        x<<-y
        m<<-NULL
      }
      get<-function()x
      setinv<-function(solve)x m<<-solve
      getinv<-function()m
      list(set=set,get=get,setinv=setinv,getinv=getinv)
}


##cacheSolve checks if the inverse of x is already cached
##if it is, then the inverse is retrieved
##if it isn't then cacheSolve finds the inverse itself and caches it.

cacheSolve <- function(x, ...) {
        m<-x$getinv
        if(!is.na(m)){
          message("retrieving cached data")
          return(m)
        }
        data<-x$get
        m<-solve(data,...)
        x$setinv(m)
        m
}
