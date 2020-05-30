makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
s <- NULL
  set <- function(y) { ##setting the value of matrix
    x <<- y
    s <<- NULL
}
  get <- function() x   ##getting the value of matrix
  setsolve <- function(solve) s <<- solve   ##setting the inverse of the matrix
  getsolve <- function() s                  ##getting the inverse of the matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}

cacheSolve <- function(x=matrix(sample(1:100,9),3,3) {
    s <- x$getsolve()                       ##Check if the inverse of the matrix is already calculated
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)                                ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  s <- solve(x)
  x$setsolve(s)                              ##Inverse of the matrix is set in the cache
  s
}
