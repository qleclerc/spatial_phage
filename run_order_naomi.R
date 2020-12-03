library(animation)

p = set_params(size=100,rep_bac=0.5,rep_bac_res=0.2,movement_bac=1,
               abx_con = 0, mutate_bac = 0.01)

bac = create_bac(100, prop_res = 0.5)
pha = create_pha(0)

bac = place_any(bac, p["size"])

times = 50

summary_data = data.frame(Time=c(0:times), Bs=0, Be=0)
summary_data[1,"Bs"] = length(which(bac$status=="S"))
summary_data[1,"Be"] = length(which(bac$status=="E"))

saveGIF({

  for(t in 2:(times+1)){

    message("Round ", t-1, " out of ", times)
    #bac = abx_kill_bac(bac, p["abx_con"])
    bac = rep_bac(bac, p["size"], p["rep_bac"], p["rep_bac_res"])
    bac = mutate_bac(bac, p["mutate_bac"])
    bac = move_any(bac, p["size"], p["movement_bac"])

    title = paste0("% of antibiotic resistant bacteria: ",
                   round(length(which(bac$status == "E"))/dim(bac)[1]*100))
    plot_all(bac, pha, p["size"],F,title=title)

    summary_data[t,"Bs"] = length(which(bac$status=="S"))
    summary_data[t,"Be"] = length(which(bac$status=="E"))
  }
}, movie.name = "test.gif", interval=0.01)


plot(summary_data$Time, summary_data$Bs, type="l", col="lightblue3",
     ylim=c(0,p["size"]*10), xlab="Time", ylab="Bacteria")
lines(summary_data$Time, summary_data$Be, col="royalblue4")


