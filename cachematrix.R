## Put comments here that give an overall description of what your
## functions do

#Function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #stores the cached inverse
        set <- function(y){
                x<<- y
                inv <<- NULL #reset cached inverse if the matrix changes
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        #return a list of the above functions
        list(set = set,get=get,setinverse=setinverse,getinverse=getinverse)
}


#Function to compute or retrieve the inverse from cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        #If the inverse is already cached, return it
        if(!is.null(inv)){
                messsage("getting cached inverse")
                return(inv)
        }
        #otherwise,calculate the inverse and cache it
        data<-x$get()
        inv<-solve(data,...)
        x$setinverse(inv)
        inv
}
