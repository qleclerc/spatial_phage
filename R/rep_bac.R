#' @title Replication of bacteria
#'
#' @description Randomly replicate bacteria, depending on space available
#'
#' @param data The dataset containing the bacteria information
#' @param size The size of the space within which to place the bacteria or phage. Must be the same as the one used to place them.
#' @param rep_bac Replication rate of bacteria
#'
#' @return An updated dataset
#'
#' @examples
#' bac_data = rep_bac(data=bac_data, size=100, rep_bac=2)
#'
#' @export

rep_bac = function(data,size=100,rep_bac=2){

  if(dim(data)[1] == 0) return(data)

  N = dim(data)[1]
  N_max = size*10

  if(N>N_max) return(data)

  rep_proba = 1-exp(-rep_bac*(1-(N/N_max)))
  rep_proba = rbinom(N, 1, rep_proba)

  for(i in 1:N){

    if(rep_proba[i] == 1) data = rbind(data, data[i,])

  }

  data
}
