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

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
