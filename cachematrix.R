## Put comments here that give an overall description of what your
## functions do.

## This function works similarly to Roger's "makeVector" example.
## It creates a list object that contains functions, accessed by '$'.
## Within these functions, the user can cache and perform operations on matrices.
## Ex.: 'mat_ob <- makeCacheMatrix()' creates a new list object.
## Ex.: 'mat_ob$setmat(matrix(rnorm(100),10,10))' creates and caches a matrix object
## populated by using rnorm().
## Ex.: 'mat_ob$invertmat(mat_ob$getmat())' inverts the matrix that was set above.

makeCacheMatrix <- function(){
  cached_inverted_matrix <- NULL 
  cached_source_matrix <- NULL
  cached_orig_matrix <- NULL
  setmat <- function(select_matrix){
    cached_orig_matrix <<- select_matrix
  }
  invertmat <- function(select_matrix){
    cached_source_matrix <<- select_matrix
    cached_inverted_matrix <<- solve(cached_source_matrix)
  }
  getmat <- function(){
    cached_orig_matrix
  }
  getsmat <- function(){
    cached_source_matrix
  }
  getimat <- function(){
    cached_inverted_matrix
  }
  list(setmat = setmat, invertmat = invertmat,
       getmat = getmat, getimat = getimat, getsmat = getsmat)
}



## cacheSolve takes as input the LIST OBJECT created by 'makeCacheMatrix()'.
## It requires that the list object already have a matrix cached using 'setmat()'.
## Then, it calculates an inverse matrix if one has not already been calculated and stored.

cacheSolve <- function(obj_matrix){
  if (is.null(obj_matrix$getmat())){
    message("No matrix set. Please use 'setmat()' to cache a matrix.")
  }
  if (!is.null(obj_matrix$getimat())){ #NULL is the default value for the inverse matrix. If not NULL, go on.
    if (mean(obj_matrix$getmat() == obj_matrix$getsmat()) == 1){
      message("Matrix identical to previous input; result already cached:")
      return(obj_matrix$getimat())
    }
    else{
      obj_matrix$invertmat(obj_matrix$getmat())
      message("New matrix. Result has been stored in cache and will print below:")
      return(obj_matrix$getimat())
    }
  }
  obj_matrix$invertmat(obj_matrix$getmat()) #If NULL, calculate the inverse and store it.
  message("No inverse yet calculated for any matrix. Result cached and will print below:")
  return(obj_matrix$getimat()) #Return the inverse matrix.
}
