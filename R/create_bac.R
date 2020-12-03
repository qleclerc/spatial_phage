#' @title Creation of "PhageSim" dataset for bacteria
#'
#' @description Creates a dataset with a specified number of bacteria to use with other functions
#'
#' @details Currently only supports 50-50 start of single resistance, need to customize
#'
#' @param num The number of bacteria in the dataset
#'
#' @return A dataset
#'
#' @examples
#' bac_data = create_bac(num=10)
#'
#' @export

create_bac = function(num=10){

  if(num %% 4 != 0) stop("Num must be even")

  x = rep(0,num)
  y = rep(0,num)
  status = c(rep("E",num*0.3),rep("S",num*0.7))
  data = data.frame(x,y,status,stringsAsFactors=FALSE)
  return(data)

}
