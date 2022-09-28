#' addFirstTwo
#'
# Description
#' Calculate the sume of the 2 first elements of a vector
#'
#' # Arguments
#' @param vec : Must be a vector with numerical values. Exemple : x or c(2,5)
#'
# Returned values
#' @return Return the sum of the 2 first elements of the vector
#'
#' @examples
#' addFirstTwo(c(1,2))
#' addFirstTwo(c(3,4,5))
#'
#' @export

addFirstTwo = function(vec){
  result = vec[1]+vec[2]
  return(result)
}

