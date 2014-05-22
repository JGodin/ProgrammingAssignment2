## The following code consists enables the creation of a special
## matrix and retrieve its cached inverse.

## This makeCacheMatrix function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}
	get<-function()x
	setinverse<-function(solve)inv<<-solve
	getinverse<-function()inv
	list(set=set, get=get, setinverse=setinverse,
			 getinverse=getinverse)

}


## This cacheSolve function computes the inverse of the special 
## matrix created by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        data<-x$get()
        inv<-solve(data, ...)
        x$setinverse(inv)
        inv
}
