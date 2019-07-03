## This function creates a special "matrix" object that can cache its inverse
## We are setting the value of matrix to Null
## Getting the new matrix
## storing the inverse of that matrix to setinverse variable
## Getting the matrix inverse by getinverse variable
## Returning all those values in list 

makeCacheMatrix <- function(x=matrix())
{
  i<- NULL
  set <- function(y)
  {
    x <<-y
    i<<- NULL
  }
  get <- function() x
  setinverse <- function(x) inv <- inv(x)
  getinverse <- function()  inv
  list(set=set,get=get,getinverse=getinverse,setinverse=setinverse)
}

## Checking whether the matrix is null or not
## If its not null then message will be displayed and inverse of matrix will be returned 

cacheSolve <- function(x,...)
{
  i <- x$getinverse()
  if(!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- inv(data,...)
  x$setinverse(i)
  i
}
b <- matrix(c(1,23,4,6),2,2)
b1<-makeCacheMatrix(b)
cacheSolve(b1)
