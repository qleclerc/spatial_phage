#' @title Placement of bacteria or phage
#'
#' @description Places the bacteria or phage randomly within a delimited space
#'
#' @param data The dataset containing the bacteria or phage information
#' @param size The size of the space within which to place the bacteria or phage
#'
#' @return An updated dataset
#'
#' @examples
#' bac_data = place_any(data=bac_data, size=100)
#'
#' @export

place_any = function(data,size=100){

  if(size*10<dim(data)[1]) stop("Not enough space! Try again with a bigger size.")


  for(i in 1:dim(data)[1]){
    data[i,"x"] = sample(0:size, 1)
    data[i,"y"] = sample(0:size, 1)
  }

  data
}
