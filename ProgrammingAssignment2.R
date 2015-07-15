
################################ Start cacheSolve ##################################   

# cacheSolve will take input as matrix parameter x for which inverse calculation need to be done.
cacheSolve <- function(x) {
  
  # I am caching x matrix in Global environment variable z. When first time cacheSolve will be called, z will not be
  # be available in Gobal environment, so search of "z" in Global enviornment will return zero length numeric type vector. 
  # If z is not available in global enviormment, initialize z as matrix with no data.
  
  if(length(grep("z",ls(envir=.GlobalEnv)))==0) # Search z in Global environment.
  {
    z<-matrix() #Initialize z as matrix with no data.
  }
  
  
  if(!is.na(z[1][1])) # Check z[1][1] is not NA. When first time cacheSolve will be called z[1][1] will be NA.
  {
    
    if(!is.null(x) & identical(x,z)) {
      print("getting cached data") # if input matrix is not null and x and already cached z is identical then take output from cache.
      return(y) # y is inverse of x and saved in Golbal enviornment variable. So, if same x matrix get called cached y will displayed and cacheSolve function will return from here.
      
    }
  }
  # if either cacheSolve function is called first time or x is either null or not identical to cached z then call makecacheMatrix 
  makeCacheMatrix(x) # pass x matrix as input to makeCacheMatrix
  
  y # display global environment variable y.
}

################################### End cacheSolve #####################################################################################################################

################################### Start makeCacheMatrix ##########################################
# makeCacheMatrix will take input as matrix parameter x for which inverse calculation need to be done.

makeCacheMatrix <- function(x=matrix()) {
  
  # x is squrare matrix for which we want to generate inverse
  setMatrix(x) # Save x in global variable z for future reference
  # Get determinant of x in d. 
  d <- det(x) 

  if(!is.na(round(d)) & round(d)>0) #If determinant is close to zero, it will be not possible to get inverse of x
  {
  y<<-solve(x) # solve is used to get inverse of x. Store inverse in y and y is global vairable can be used by any other function.
  }
  else
  {
    print("Not a valid square Matrix. Either determinant is less than or equal to zero or NA!!")
    y<<-matrix() # In case of invalid square matrix as input set y with matrix with no data.
  }
  
}
################################# End makeCacheMatrix #########################################

##################################### Start setMatrix ########################################
 setMatrix <- function(x=matrix()) {
    # we are storing matrix x in z for future reference.It will use to generate cached inverse in case matrix data is not changed.
    z<<-x
  }
##################################### End setMatrix #################################################
   
####################################  Start getMatrix ########################################## 
 getMatrix <- function() {z} # return z
 
##################################### End getMatrix #########################################
