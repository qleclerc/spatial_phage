#' @title Creation of "PhageSim" dataset for bacteria
#'
#' @description Creates a dataset with a specified number of bacteria to use with other functions
#'
#' @param num The number of bacteria in the dataset
#' @param prop_res Starting proportion of bacteria that are resistant to antibiotics
#'
#' @return A dataset
#'
#' @examples
#' bac_data = create_bac(num=10, prop_res = 0.5)
#'
#' @export

create_bac = function(num=10, prop_res = 0.5){

  x = rep(0,num)
  y = rep(0,num)
  status = c(rep("E",floor(num*prop_res)),
             rep("S",ceiling(num*(1-prop_res))))
  data = data.frame(x,y,status,stringsAsFactors=FALSE)
  return(data)

}
