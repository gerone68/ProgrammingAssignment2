## Put comments here that give an overall description of what your functions do
## Write a short comment describing this function
#This function will retrieve and store the different matrices. These are the 
#actual matrix, the last stored matrix and the inverse of the last stored matrix.
#The list of function can be called in cacheSolve
makeCacheMatrix <- function(x = matrix()) {
	#get the original matrix	
		getOriginal<-function() x
	#get the stored matrix	
		getStored<-function() y
	#get the stored inverse matrix	
		getInverse<-function() z
	#set the original matrix
		setOriginal<-function(y) y<<-y
	#set the inverse matrix
		setInverse<-function(z) z<<-z
	list(getOriginal=getOriginal,getStored=getStored,getInverse=getInverse,
		setOriginal=setOriginal,setInverse=setInverse)
}

## Write a short comment describing this function
##This function will get the actual matrix and then see if the stored matrix is identical.
##if this is true, we can retrieve the stored inverse matrix and otherwhise we need to solve
cacheSolve<-function(x) {
  ## Return a matrix that is the inverse of 'x'
   ##get the actual matrix	
	MO<-x$getOriginal()	
   ##get the stored matrix of which we have calculated the inverse
	MS<-x$getStored()	 	
	##Check if the actual matrix is identical to the stored matrix	
	if (identical(MO,MS)) {
		##Then we can retrieve the already calculated inverse matrix
		INV<-x$getInverse()
		return(INV)
	}
   ##Otherwise calculate the inverse matrix
	INV<-solve(MO)
	#then store the actual matrix and the inverse matrix
		x$setOriginal(MO)
		x$setInverse(INV)
		INV
}

