#' @title Creation of "PhageSim" dataset for phage
#'
#' @description Creates a dataset with a specified number of phage to use with other functions
#'
#' @param num The number of phage in the dataset
#'
#' @return A dataset
#'
#' @examples
#' pha_data = create_pha(num=10)
#'
#' @export

create_pha = function(num=10){

  x = rep(0,num)
  y = rep(0,num)
  status = rep("P",num)
  data = data.frame(x,y,status,stringsAsFactors=FALSE)
  return(data)

}
