## Create function makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.
## An object if this type will be created, which will return inverse matrix from cached data, if computed earlier

makeCacheMatrix <- function(x = matrix()) 
{
    inverseOfMatrix <- NULL
    
      set_Matrix <- function(y)			#setting value of the Matrix 
	  {				    	
			inverseOfMatrix <<- NULL	# This variable will store the matrix
			x <<- y
      }
      
      get_Matrix <- function()  		#getting value of the Matrix
	  {
			x
	  } 

	  set_Inverse <- function(inverse) 	#setting value of the inverse matrix
	  {
			inverseOfMatrix <<- inverse  
	  } 	

	  get_Inverse <- function() 		#getting value of the invertible matrix
	  {
			inverseOfMatrix             
      }  

	  list(set_Matrix = set_Matrix, get_Matrix = get_Matrix, 
			set_Inverse = set_Inverse, get_Inverse = get_Inverse)

} ## End of makeCacheMatrix function


## This function cachesolve() gives inverse of the matrix created by makeCacheMatrix function.
## If the matrix definition is not changed and the inverse has been calculated, then
## the inverse matrix is given from the cached data. Else it is computed afresh and
## loaded in cache (to be retrieved later, if same matrix is used).

cacheSolve <- function(x, ...) 
{
    #get inverse of matrix from makeCacheMatrix function
	
        inverseOfMatrix <- x$get_Inverse()
        if(is.null(inverseOfMatrix)) 	
		{                       
              MatrixData <- x$get_Matrix()                     		# Original Matrix 
              inverseOfMatrix <- solve(MatrixData, ...)             # Perform Matrix Inversal 
              x$set_Inverse(inverseOfMatrix)                        # Set Inverse matrix 
              return(inverseOfMatrix)                               # Return Inverse matrix of x
   		} else {
				print("Getting Inverse of Matrix from Cached Data")   	# If inverse matrix is not NULL, then 
                return(inverseOfMatrix)                                 # Get the value from cached data
		}
}


### Function test results ###

#My_Matrix <- matrix(1:4,2,2)
#print(My_Matrix)
#      [,1] [,2]
#[1,]    1    3
#[2,]    2    4

#makeCache_Object <- makeCacheMatrix(My_Matrix)

#makeCache_Object$get_Matrix()
#      [,1] [,2]
#[1,]    1    3
#[2,]    2    4

#makeCache_Object$get_Inverse()
# NULL

#cacheSolve(makeCache_Object)
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#cacheSolve(makeCache_Object)

#"Getting Inverse of Matrix from Cached Data"
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#Note -- Exercise care while declaring the matrix. It should be a Square Matrix and cannot be a Singular Matrix
