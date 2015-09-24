## The makeCacheMatrix and cacheSilve function are created for 
# Coursera R-programming course Programmin Assignemt 2.
#
# The combination of the two function allows to cache 
# matrix inversion calculation results and if the same matrix
# inversion is required again, the result is take out from cache, 
# rather than being calculated again.
#
# Version 24/09/2015


## MakeCacheMatrix function 
# The purpose of the function is
# to produce a list of functions, that allow to store (set)
# the data, callout the data (get), calculate and cache the 
# inverse matrix (setinv) and get inverse matrix from cache
# (getinv)
#


makeCacheMatrix <- function(x = matrix()) {
        # assign NULL to inverted matrix        
        invm<-NULL
        
        # define the set function which saves the input matrix
        # note when set function is called invm is intialised 
        # as NULL,       
        set<-function(y){
                x <<- y
                invm <<- NULL
        }
        
        # get function to get the data out        
        get<-function () x
        
        # setinv is the function to calculate the inverse 
        # matrix in case it hasn't been cashed before        
        setinv<-function (solveres) invm<<-solveres
        
        # get previously cashed inverse matrix
        getinv<-function () invm
        
        # a list of functions - output of the makeCacheMatrix 
        list(set=set, get=get,
             setinv=setinv, getinv=getinv)
}


## cacheSolve function 
# The purpose of the function is
# to check if matrix inversion for this particular matrix
# has already been done and if yes, invm variable isn't NULL
# and will be recovered from cache. If this is the first
# time dealing with the matrix inverse matrix will be 
# calculate and assigned to invm.
#

cacheSolve <- function(x, ...) {
   
        
# getting invm out of the "matrix", basically called out by
# getinv function
        invm <- x$getinv()
        
# check if invm is NULL or has a value. If it's not NULL
# cached value of inversion matrix is returned        
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        
# if invm is NULL, the matrix is assigned to data 
# and inverse matrix calculation is carried out
# the result of the calculation is stored in cache 
# for future use using setinv function
        message("calculating inverse matrix")
        data <- x$get()
        invm <- solve(data, ...)
        x$setinv(invm)
        invm
}
