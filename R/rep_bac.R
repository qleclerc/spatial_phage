#' @title Replication of bacteria
#'
#' @description Randomly replicate bacteria, depending on space available
#'
#' @param data The dataset containing the bacteria information
#' @param size The size of the space within which to place the bacteria or phage. Must be the same as the one used to place them.
#' @param rep_bac Replication rate of bacteria
#' @param rep_bac_res Replication rate of resistant bacteria
#'
#' @return An updated dataset
#'
#' @examples
#' bac_data = rep_bac(data=bac_data, size=100, rep_bac=2,rep_bac_res=2)
#'
#' @export

rep_bac = function(data,size=100,rep_bac=2,rep_bac_res=0){

  if(dim(data)[1] == 0) return(data)

  N = dim(data)[1]
  N_max = size*10

  if(N>N_max) return(data)

  rep_proba = 1-exp(-rep_bac*(1-(N/N_max)))
  Ns = which(data$status == "S")
  rep_proba = rbinom(length(Ns), 1, rep_proba)

  rep_proba_res = 1-exp(-rep_bac_res*(1-(N/N_max)))
  Ne = which(data$status == "E")
  rep_proba_res = rbinom(length(Ne), 1, rep_proba_res)

  for(i in 1:length(Ns)){
    if(rep_proba[i] == 1) data = rbind(data, data[Ns[i],])
  }

  for(i in 1:length(Ne)){
    if(rep_proba_res[i] == 1) data = rbind(data, data[Ne[i],])
  }

  data
}
