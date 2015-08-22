# Function makeCacheMatrix
#   Construct a special matrix list representing a matrix and its inverse.
#   The list is composed of functions to set the value of the matrix,
#   retrieve its value, set the inverse of the matrix, and retrieve the inverse.

makeCacheMatrix<-function(matx=matrix()) {
      matx_inv<-NULL					# Initialize the matrix inverse to NULL

	set<-function(y) {				# (Re)set the elements of the matrix and initialize
		matx<<-y				#   the value of its inverse to NULL
		matx_inv<<-NULL				# NOTE: set() is not really used in this assignment,
	}						#   but it may be useful as a counterpart to get()

	get<-function() matx				# Return the elements of the matrix
	setinv<-function(inv) matx_inv<<-inv		# Set the matrix inverse (in the parent environment)
      getinv<-function() matx_inv			# Retrieve the inverse of the matrix
	list(set=set,get=get,setinv=setinv,getinv=getinv)	# Construct the special matrix list
}

# Function cacheSolve
#   Return the cached inverse of the matrix represented by the special matrix list
#   matx_list if that inverse has already been computed and cached. Otherwise, compute 
#   the inverse, cache it, and return it.

cacheSolve<-function(matx_list,...) {
	matx_inv<-matx_list$getinv()			# Return either the cached value of the matrix inverse or
							#   NULL if the inverse has not previously been cached
	if(!is.null(matx_inv)) {			# If the inverse is already cached, return the cached value
		message("getting cached data")		#   and exit cacheSolve
		return(matx_inv)
	}

	# If the inverse has not already been cached ...
	data<-matx_list$get()				# Retrieve the matrix elements
	matx_inv<-solve(data, ...)			# Compute the inverse of the matrix
							#   NOTE: This code assumes the given matrix is invertible
	matx_list$setinv(matx_inv)			# Cache the matrix inverse
	matx_inv					# Return the matrix inverse
}
