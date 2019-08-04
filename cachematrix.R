## Put comments here that give an overall description of what your
## functions do

## This function creates a list representing the "special matrix" object that can cache its inverse
## We are representing the item as a list with 4 functions:
## setMatrix: will store the original matrix in x variable (using that name due to exercise constraint)
## getMatrix: will return the original matrix stored in x variable (using that name due to exercise constraint)
## getInverseMatrix: will return the inverse in case it was calculated before (inverseMatrix variable). It will return NULL if not
## setInverseMatrix: will store the calculated inverse matrix into the inverseMatrix variable

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  getMatrix <- function() x
  setInverseMatrix <- function(inverseMatrix) inverseMatrix <<- inverseMatrix
  getInverseMatrix <- function() inverseMatrix
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix
  )
}

## This function tries to get the inverse matrix from cache. If not
## it will calculate, store the new value in x variable and return the value.
## Lines 35 to 39 are the ones that get the cached value. If the value was calculated before, it returns it
## Rest of the code get the original matrix, calculates the inverse
## stores it in the x variable using the method provided and returns the value in line 45.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cachedInverseMatrix <- x$getInverseMatrix()
    if (!is.null(cachedInverseMatrix)) {
      message("Returning Cached Inverse Matrix")
      return(cachedInverseMatrix)
    }
    originalMatrix <- x$getMatrix()
    calculatedInverseMatrix <- solve(originalMatrix)
    x$setInverseMatrix(calculatedInverseMatrix)
    calculatedInverseMatrix
}
