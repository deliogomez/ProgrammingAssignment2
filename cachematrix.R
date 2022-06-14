## Put comments here that give an overall description of what your
## functions do
## Function set_mat saves the current matrix
## Function set_inv saves calculated inverse matrix
## Function get_mat retrieves saved matrix
## Function get_inv retrieves saved inverse matrix
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  set_mat <- function(y) mat <<- y
  set_inv <- function(y) inv <<- y
  get_mat <- function() mat
  get_inv <- function() inv
  list(set_mat = set_mat, set_inv = set_inv,
       get_mat = get_mat, get_inv = get_inv)
 }


## Write a short comment describing this function
## cachesolve instantiates the functions defined in makeCacheMatrix during the first execution
## calculates the inverse matrix from the function argument matrix
## and saves the argument matrix and the corresponding inverse matrix
## in the first execution or if the argument matrix is different
## from the saved matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
      if (!(exists("first"))) { #is the first execution?
        matinv <<- makeCacheMatrix(x)  #
        first <<- TRUE
        new_inv <- solve(x)
        matinv$set_mat(x)  #Saves argument matrix
        matinv$set_inv(new_inv) #Saves calculated inverse matrix
        return(new_inv)
      } else {
          old_mat <- matinv$get_mat() #Retrieve saved matrix
          if(identical(x, old_mat)) {  #Is new argument matrix the same as saved matrix?
            old_inv <- matinv$get_inv() #Retrieve saved inverse matrix
            message("Getting cached matrix")
            return(old_inv)
          } else {   #New argument matrix different from saved matrix
            new_inv <- solve(x)
            matinv$set_mat(x)
            matinv$set_inv(new_inv)
            return(new_inv)
        } 
        } 
}
