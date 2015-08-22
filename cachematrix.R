## Cache the inverse of a matrix and use the cache when the original matrix matches


##  Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Returns a list containing functions to access input matrix and cache
    
    # The orginal-inverse matrix pair
    originalMatrix <- NULL
    inverseMatrix <- NULL
    
    # The methods to set/get the input matrix
    set <- function(y) {
        x <<- y
        originalMatrix <- NULL
        inverseMatrix <<- NULL
    }
    
    get <- function() x
    
    # The methods to set/get orginal  matrix that is related to the inverse matrix
    setOriginalMatrix <- function(m) originalMatrix <<- m
    getOriginalMatrix <- function() originalMatrix
    
    # The methods to set/get inverse matrix
    setInverseMatrix <- function(m) inverseMatrix <<- m
    getInverseMatrix <- function() inverseMatrix
    
    # Returns the methods as a list
    list(set = set, get = get,
         
         setOriginalMatrix= setOriginalMatrix,
         getOriginalMatrix = getOriginalMatrix,
         
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## Gets the inverse from the cache or computes the inverse on the spot.

cacheSolve <- function(x, ...) {
    # Returns a matrix that is the inverse of 'x'
    
    inputMatrix = x$get()
    originalMatrix = x$getOriginalMatrix()
    
    # Checks if the input matrix has been changed and if its inverse has been cached
    if(identical(inputMatrix, originalMatrix)) {
        inverseMatrix <- x$getInverseMatrix()
        
        if(!is.null(inverseMatrix)) {
            return(inverseMatrix)
        }
    }
    
    # Computes the reverse matrix
    inverseMatrix <- solve(inputMatrix, ...)

    # Caches the result
    x$setOriginalMatrix(inputMatrix)
    x$setInverseMatrix(inverseMatrix)
    
    inverseMatrix   
}
