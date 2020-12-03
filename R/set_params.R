#' @title Create the transmission kernel parameters
#'
#' @description This is needed to execute the update_all and simulate_all functions
#'
#' @param size The size of the space within which the bacteria and phage can move.
#' @param rep_bac Replication rate of bacteria
#' @param rep_bac_res Replication rate of resistant bacteria
#' @param movement_bac The maximum distance that bacteria can move during one generation
#' @param movement_pha The maximum distance that phage can move during one generation
#' @param beta Phage infection probability
#' @param burst Phage burst size
#' @param trans Probability of transduction instead of normal replication
#'
#' @return A vector containing the parameter values
#'
#' @examples
#' params = set_params()
#'
#' @export

set_params = function(size=250,rep_bac=2,rep_bac_res=2,movement_bac=10,
                      abx_con=0,mutate_bac=0,
                      movement_pha=10,beta=0.5,burst=10,trans=0.1,decay=0.5){

  if(abs(movement_pha-round(movement_pha)) >
     .Machine$double.eps^0.5) stop("Phage movement parameter must be an integer!")
  if(abs(movement_bac-round(movement_bac)) >
     .Machine$double.eps^0.5) stop("Bacteria movement parameter must be an integer!")
  if(beta > 1) stop("Phage lytic probability must be less than 1!")
  if(trans > 1) stop("Phage transduction probability must be less than 1!")


  params=c(size=size,rep_bac=rep_bac,rep_bac_res=rep_bac_res,movement_bac=movement_bac,
           abx_con=abx_con,mutate_bac=mutate_bac,
           movement_pha=movement_pha,beta=beta,burst=burst,trans=trans,decay=decay)

  return(params)

}
