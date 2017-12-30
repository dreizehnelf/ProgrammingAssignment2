# These two functions allow to calculate the inverse of a matrix and cache the
# result for subsequent calls to cacheSolve.


# makeCacheMatrix creates an object that provides a caching mechanism for the
# calculation of the inverse of a matrix as well as a couple of utility methods
# to easily use it.
makeCacheMatrix <- function(the_matrix = matrix()) {

  # first we define our internal cache variable and set it to NULL
  # so we know that we do not have a cached inverse yet
  cached_inverse <- NULL
  
  # we also create a variable to hold the actual matrix data for
  # later reference
  matrix_data <- the_matrix
  
  # then we define a couple of helper methods
  
  # 1. a method to check if we have a cached value or not
  has_cached_inverse <- function() {
    return(!is.null(cached_inverse))
  }
  
  # 2. the method to cache a result
  cache <- function(the_inverse) {
    cached_inverse <<- the_inverse
  }
  
  # 3. setter method to reset the cache if the matrix changes
  set_matrix <- function(the_matrix = matrix()) {
    
    # only replace the value and clear the cache if the new
    # matrix is actually different from the one we have
    if(!identical(the_matrix, matrix_data)) {
      matrix_data <<- the_matrix
      cached_inverse <<- NULL
    }    
  }
  
  # 4. getter method for the matrix data
  get_matrix <- function() {
    return(matrix_data)
  }
  
  # 5. getter method for the inverse (with cache check/calculation)
  get_inverse <- function() {
    
    # if there is no cached inverse, we compute and cache it
    if(is.null(cached_inverse)) {
      print("[makeCacheMatrix] Fresh calculation.")
      cache(solve(matrix_data))
    } else {
      print("[makeCacheMatrix] Cache hit.")
    }
    
    # and then return the cached value
    # (which should now be guaranteed to be there)
    return(cached_inverse)
  }

  # 6. getter method for the cached inverse (without calculation)
  get_cached_inverse <- function() {
    return(cached_inverse)
  }
  
  # finally we return a list with our helper functions, so we can
  # use it as our special matrix
  return(list(
    set = set_matrix,
    get = get_matrix,
    inverse = get_inverse,
    cached_inverse = get_cached_inverse,
    has_cached_inverse = has_cached_inverse
  ))
  
}

# cacheSolve will calculate the inverse of an instance of a cache matrix object
# created by makeCacheMatrix. If the inverse is available via cache, it will use
# that one, otherwise it will calculate a fresh version
cacheSolve <- function(x, ...) {

  # first we check, whether we have a cached inverse available
  if(x$has_cached_inverse()) {
    # if so, we return it
    print("[cacheSolve] Cache hit.")
    return(x$cached_inverse())
  } else {
    # otherwise, we calculate it, cache it and return it
    print("[cacheSolve] Fresh calculation.")
    return(x$inverse())
  }
  
  # we could also have just left out the whole if/else block here, since
  # a "return(x$inverse()) would have done the same job. But the exercise
  # required the cacheSolve method to contain the check for a cached inverse
  # in itself, so it is there :)
}

