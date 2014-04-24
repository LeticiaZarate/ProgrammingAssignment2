## makeCacheMatrix is a function that takes a matrix 'x' as an argument and 
## returns an especial 'matrix' which is a list of functions over the matrix.
## These functions are: 
## 1.- set: creates a new matrix
## 2.- get: recover the value of the matrix
## 3.- setinverse: saves the inverse of 'x'
## 4.- getinverse: recover the value of the inverse of 'x'

## cacheSolve is a function that takes a special object as argument (a list as created by MakeCacheMatrix)
## and returns the inverse of the matrix if stored or calculates it and saves it in the object before returning it. 


## Create a special 'matrix', which a list containing a function to set the matrix value, a function to get it,
## a function to save its inverse and a function to get the inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## 'x' is an invertible matrix
  
  m_inv <- NULL # set the inverse value of the 'x' matrix to NULL 
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  } # stores a new matrix 'y' and sets the inverse of this matrix to NULL
  get <- function() x # return the value of the original matrix
  setinverse <- function(inverse) m_inv <<- inverse # stores the matrix 'inverse' as the inverse of the matrix
  getinverse <- function() m_inv # return the value of the stored inverse value of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) #returns a list of functions on the matrix 'x'
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  ## 'x' is an especial 'matrix' as the ones returned by makeCacheMatrix function 
  
  m_inv <- x$getinverse() #gets the stored value of the inverse matrix of 'x'
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  } #if a stored value exists, prints a message and returns its value
  data <- x$get() #recover the original matrix from the special one
  m_inv <- solve(data, ...) #calculate the inverse of the matrix
  x$setinverse(m_inv) #stores the inverse of the matrix in the special 'matrix'  
  m_inv #returns a matrix which is the value of the inverse matrix
}
