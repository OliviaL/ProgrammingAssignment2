## Cache inverse of a Matrix
## overall, the functions make the Matrix and get the inverse of it

## first, it sets a function to get the matrix which can cache inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m=NULL
  set <- function (y){
    x <<-y
    m <<- NULL
  }
  get <-function ()x
  
  setinverse <-function(solve) m <<- solve
  getinverse <- function () m
  list(set=set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## computes inverse of the matrix above.  If inverse is already calculated, retrieve inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
  m <- x$getinverse()
  if(!is.null(m)){
    message ("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <-solve(data, ...)
  x$setinverse(m)
  m
}

