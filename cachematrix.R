## Put comments here that give an overall description of what your
## functions do
## TODO rebuild from scratch, that way you learn the steps
## 
## Write a short comment describing this function

##	create a "matrix" object that can cache its inverse
##	matrix object provides functions to 
## 	get() to get matrix value
##	set() to set matrix value
##	getinverse() to get matrix inverse (precalculated/stored/cached value)
##	setinverse() to set matrix inverse (to calculate and cache/store a new value)


## 	responsibilities to determine whether matrix has changed or not?
##	flagchange state will be mantained by the object:
##	true indicates that the matrix hasn't changed
##
makeCacheMatrix <- function(x = matrix()) {
	# 	x is passed in at time of creation
	#	initialize the cached inverse to Null at time of creation
	inverse<-NULL
	#	create a getter function that gets the value of the matrix
	get <- function() x
	#	create a setter function that gets the value of the matrix
	#	if the matrix has changed then we need to set inverse to NULL
	set <- function(y = matrix())
	{
		
		# first create a helper function to see whether the two matrices are equal
		matequal <- function(x, y)
			is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
		
		#	do a quick check, just in case y is exactly equal to x
		#	then we don't need to do anything, x and inverse will remain valid
		#	if they are not equal then set x to new value and update inverse to NULL
		if(matequal(x,y)==FALSE){
			x<<-y
			inverse<<-NULL	
		}
	}
	#	gets the stored value of the matrix inverse
	getinv <- function() inverse
	setinv <- function(y=matrix()) {
		inverse<<-y
	}
	list(get=get,set=set, getinv=getinv, setinv=setinv)
}


## Write a short comment describing this function
## 
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	## If the current inverse is vald then return it, else calculate the inverse, store it and return it
	if(!is.null(x$getinv())){
		##	this is just a debug print statement to show that cached value is being accessed
		##	can be removed in final version
		print("getting cached value")
		x$getinv()
	}
	else {
		##	calculate the matrix inverse and store it
		data<-x$get()
		inverse<-x$setinv(data)
		##	return the calculated inverse
		inverse
	}
		
}
