#' @title Update bacteria infections by phage
#'
#' @description Updates the bacteria infections by phage, generating or decaying phage
#'
#' @param data_bac The dataset containing the bacteria information
#' @param data_pha The dataset containing the phage information
#' @param p General parameters. These must have been created using the set_params function.
#'
#' @return An updated dataset
#'
#' @examples
#' dots_data = update_dots(data=dots_data, gen=0, p=params)
#'
#' @export

update_all = function(data_bac,data_pha,p,tr_count=0){

  num_pha=dim(data_pha)[1]

  if(dim(data_bac)[1] != 0 && num_pha != 0){

    coord_bac = paste0(data_bac$x,".",data_bac$y)
    coord_pha = paste0(data_pha$x,".",data_pha$y)

    good_bac = which(coord_bac %in% coord_pha)
    good_pha = which(coord_pha %in% coord_bac)

    for(i in good_bac){

      for(j in good_pha){

        if(data_pha[j,"status"] != "gone"){

          if(data_bac[i,"x"]==data_pha[j,"x"] && data_bac[i,"y"]==data_pha[j,"y"]){

            if(data_bac[i,"status"] == "M"){
              data_pha[j,"status"] = "gone"
              next()
            }

            if(p["beta"]>runif(1)){

              if(data_pha[j,"status"] == "P"){
                data_pha = rbind(data_pha, do.call("rbind", replicate(p["burst"]-1,data_pha[j,], simplify=F)))

                if(p["trans"]>runif(1) && data_bac[i,"status"]=="E"){
                  data_pha[j,"status"] = data_bac[i,"status"]
                }

                data_bac[i,"status"] = "lysed"
                break()

              } else {

                if(data_bac[i,"status"] == "S"){
                  data_bac[i,"status"] = data_pha[j,"status"]
                  assign("tr_count", tr_count+1, envir = .GlobalEnv)
                }
                data_pha[j,"status"] = "gone"
                break()

              }
            }
          }
        }
      }
    }
  }

  if(num_pha !=0){

    die_proba = rbinom(num_pha, 1, p["decay"])

    for(i in 1:num_pha){

      if(die_proba[i] == 1) data_pha[i,"status"] = "gone"

    }

  }

  if("gone" %in% data_pha$status) data_pha = data_pha[-(which(data_pha$status == "gone")),]
  if("lysed" %in% data_bac$status) data_bac = data_bac[-(which(data_bac$status == "lysed")),]

  assign("bac", data_bac, envir=.GlobalEnv)
  assign("pha", data_pha, envir=.GlobalEnv)


}
