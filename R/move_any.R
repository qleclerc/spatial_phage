#' @title Movement of bacteria or phage
#'
#' @description Randomly moves bacteria or phage within the delimited space
#'
#' @param data The dataset containing the bacteria or phage information
#' @param size The size of the space within which to place the bacteria or phage. Must be the same as the one used to place them.
#' @param movement The maximum distance that bacteria or phage can move during one generation
#'
#' @return An updated dataset
#'
#' @examples
#' bac_data = move_any(data=bac_data, size=100, movement=5)
#'
#' @export

move_any = function(data,size=100,movement=5){

  if(dim(data)[1] == 0) return(data)

  for(i in 1:dim(data)[1]){

    new_x = data[i,"x"]+sample(-movement:movement,1)
    new_y = data[i,"y"]+sample(-movement:movement,1)

    if(new_x>size){
      new_x = size
    }
    if(new_x<0){
      new_x = 0
    }
    if(new_y>size){
      new_y = size
    }
    if(new_y<0){
      new_y = 0
    }
    data[i,"x"] = new_x
    data[i,"y"] = new_y
  }

  data
}
