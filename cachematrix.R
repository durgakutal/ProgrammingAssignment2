#makeCacheMatrix: This function creates a special "matrix" object that can cache
#its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned
#by makeCacheMatrix above. If the inverse has already been calculated (and the 
#matrix has not changed), then the cachesolve should retrieve the inverse from 
#the cache.
#Computing the inverse of a square matrix can be done with the solve function
#in R. For example, if X is a square invertible matrix, then solve(X) returns
#its inverse.

makeCacheMatrix<-function(m=matrix()){
  ##To initialize the inverse property
    inv<-NULL
    set<-function(x){
            m<<-x
            inv<<-NULL
    }
    ##To get the matrix
    get<-function(){
    	m
    }
    ##The way to set the inverse of the matrix
    setInverse<-function(inverse){
        inv<<-inverse
    }

    ##The way to get the inverse of the matrix
    getInverse <- function() {
        inv
    }
    ##Return a list of the methods
    list(set=set,
         get=get,
         setInverse=setInverse,
         getInverse=getInverse)
    }
    ##Computing the inverse of the unique matrix back by "makeCacheMatrix"
    cacheSolve<-function(m,...){
    ##Return a matrixthat is the inverse of m
    inv<-m$getInverse()
    if(!is.null(inv)){
            message("getting cached data")
            return(inv)
    }
    y<-m$get()
    inv<-solve(y,...)
    m$setInverse(inv)
    inv
    }
