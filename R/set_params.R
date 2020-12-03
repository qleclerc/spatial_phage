#' @title Create the transmission kernel parameters
#'
#' @description This is needed to execute the update_all and simulate_all functions
#'
#' @param size The size of the space within which the bacteria and phage can move.
#' @param rep_bac Replication rate of bacteria
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

set_params = function(size=250,rep_bac=2,movement_bac=10,movement_pha=10,beta=0.5,burst=10,trans=0.1,decay=0.5){

  params=c(size=size,rep_bac=rep_bac,movement_bac=movement_bac,movement_pha=movement_pha,
           beta=beta,burst=burst,trans=trans,decay=decay)

  return(params)

}
