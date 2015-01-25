## Programming assignment by Anil Raut (anilraut30@gmail.com)

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix() returns a "function list" which upon passing to cacheSolve method dynamically executes functions such as
## get, set, getmatrix and setmatrix

makeCacheMatrix <- function(x = matrix()) {

  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
  
}


## The program calculates Inverse for Sqaure matrix only. 
## It ensures the input matrix supplied is a Sqaure matrix. if not, it shows message
## It validates if the matrix is iversible or not, and shows message otherwise instead of showing system error message

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached matrix")
    return(m)
  }
  matrix<-x$get()
  
  if(nrow(matrix) == ncol(matrix))
  {
    
    if(class(try(solve(matrix),silent=T))=="matrix")
    {
      m<-solve(matrix, ...)
      x$setmatrix(m)
      m
    }
    else
      message("Input matrix is not inversible, pls try with different square matrix!")
  
  }
  else
    message("Input matrix is non sqaure matrix, please provide Square matrix and try again!")
}

## Output Tests

## test 1 

#> m <- matrix(1:4,2,2)
#> mat <- makeCacheMatrix(m)
## displays the matrix by building and storing in cache
#> cacheSolve(mat)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

## in 2nd attempt the matrix displays the matrix from Cache
#> cacheSolve(mat)
#getting cached matrix
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

## Test 2 - with non sqaure matrix  e.g. 3x2 matrix

#> m <- matrix(1:6,3,2)
#> mat <- makeCacheMatrix(m)
#> cacheSolve(mat)
#Input matrix is non sqaure matrix, please provide Square matrix and try again!



## Test 3

#> m <- matrix(1:9,3,3)
#> mat <- makeCacheMatrix(m)
#> cacheSolve(mat)
#Input matrix is not inversible





