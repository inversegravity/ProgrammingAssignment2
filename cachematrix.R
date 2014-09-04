# Summary:  A pair of functions, makeCacheSolve and cacheSolve.  makeCacheSolve
#   sets up a list of formulas required as input to cacheSolve.  cacheSolve
#   calculates the inverse of a matrix, assigns that result to "cache" (i.e.
#   stores the value in the global environment using the <<- operator) and, on
#   subsequent calls, reads the cache rather than doing the work of 
#   recalculating.
#
#   Depending on the size of the original input matrix this technique can save
#   processing e.g. in loops where the same inverse would otherwise be
#   unnecessarily recalculated repeatedly from a non-changing input matrix.
#

makeCacheSolve <- function(originalMatrix = matrix()) {
	# Creates a list containing functions required by cacheSolve.
	#
	# Args:
	#   originalMatrix: the numeric matrix to be inverted; must be invertable.
	#
	# Returns: 
	#   A list consisting of function definitions.  The list is required as 
	#   input to cacheSolve.  Functions defined in the list are as follows:
	#       resetOriginalMatrix: set the value of the original matrix to a new 
	#                            value and initialize cachedInverse = NULL. 
	#       getOriginalMatrix:   get the value of the original matrix.
	#       setCachedInverse:    set the value of the inverse matrix.
	#       getCachedInverse:    get the value of the inverse matrix.
	
	cachedInverse <- NULL  
	
	resetOriginalMatrix <- function(newMatrix = matrix()) {
		originalMatrix <<- newMatrix
		cachedInverse <<- NULL
	}
	
	getOriginalMatrix <- function() {
		originalMatrix   
	}
	
	setCachedInverse <- function(inverseMatrix) {
		cachedInverse <<- inverseMatrix
	}
	
	getCachedInverse <- function() {
		cachedInverse
	}
	
	# return a list containing the above functions:
	list( resetOriginalMatrix = resetOriginalMatrix,
	      getOriginalMatrix   = getOriginalMatrix,
	      setCachedInverse    = setCachedInverse,
	      getCachedInverse    = getCachedInverse
	)
}

cacheSolve <- function(funcList, ...) {
	# Returns the value of the inverse matrix by first calculating it, storing 
	# it and, on subsequent calls, by reading the cached value.  Uses standard 
	# function solve so accepts the same optional arguments as solve.
	#
	# Args:  
	#   funcList: list containing functions created by makeCacheSolve.
	#
	# Returns: 
	#   inv: inverse matrix, read from cache when possible.
	
	inv <- funcList$getCachedInverse()
	if (!is.null(inv)) {
		message("returning cached value")
		return(inv)
	}
	origMatrix <- funcList$getOriginalMatrix()
	inv <- solve(origMatrix, ...)
	funcList$setCachedInverse(inv)
	inv
}
