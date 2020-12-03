#' @title Mutation of bacteria
#'
#' @description Randomly mutate bacteria, depending on mutation rate
#'
#' @param data The dataset containing the bacteria information
#' @param mutate_bac Mutation rate of bacteria
#'
#' @return An updated dataset
#'
#' @examples
#' bac_data = mutate_bac(data=bac_data, mutate_bac=2)
#'
#' @export

mutate_bac = function(data, mutate_bac=0.1){

  mutate_proba = 1-exp(-mutate_bac)

  N = which(data$status == "S")

  rep_proba = rbinom(length(N), 1, mutate_proba)

  for(i in 1:length(N)){
    if(rep_proba[i] == 1) data$status[N[i]] = "E"
  }

  data
}
