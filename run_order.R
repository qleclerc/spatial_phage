library(animation)

p = set_params(size=100,rep_bac=0.5,movement_bac=0.5,movement_pha=3,beta=1,burst=20,trans=0,decay=0.28)

#seed_num=6
#from 25 to 50
#p = set_params(size=100,rep_bac=0.5,movement_bac=1,movement_pha=2,beta=1,burst=10,trans=0.9,decay=0.3)

#set.seed(seed_num)

bac = create_bac(1000)
pha = create_pha(50)

bac = place_any(bac, p["size"])
pha = place_any(pha, p["size"])
#plot_all(bac, pha, p["size"])

times = 100

summary_data = data.frame(Time=c(0:times), Bs=0, Be=0, P=0, Pe=0)
summary_data[1,"Bs"] = length(which(bac$status=="S"))
summary_data[1,"Be"] = length(which(bac$status=="E"))
summary_data[1,"P"] = length(which(pha$status=="P"))
summary_data[1,"Pe"] = length(which(pha$status=="E"))

tr_count=0

saveGIF({

  for(t in 2:(times+1)){

    if(dim(pha)[1]==0){
      pha = create_pha(10)
      pha = place_any(pha, p["size"])
    }

    if(dim(bac)[1]==0){
      bac = create_bac(10)
      bac = place_any(bac, p["size"])
    }


    message("Round ", t-1, " out of ", times)
    bac = rep_bac(bac, p["size"], p["rep_bac"])
    bac = move_any(bac, p["size"], p["movement_bac"])
    pha = move_any(pha, p["size"], p["movement_pha"])
    title = paste0("% of antibiotic resistant bacteria: ", round(length(which(bac$status == "E"))/dim(bac)[1]*100))
    plot_all(bac, pha, p["size"],F,title=title)

    update_all(bac, pha, p, tr_count)

    #plot_all(bac, pha, p["size"],F)

    summary_data[t,"Bs"] = length(which(bac$status=="S"))
    summary_data[t,"Be"] = length(which(bac$status=="E"))
    summary_data[t,"P"] = length(which(pha$status=="P"))
    summary_data[t,"Pe"] = length(which(pha$status=="E"))
  }
}, movie.name = "test.gif", interval=0.01)


par(mar=c(5,4,4,5)+.1)

plot(summary_data$Time, summary_data$Bs, type="l", col="royalblue1", ylim=c(0,p["size"]*10), xlab="Time", ylab="Bacteria & transducing phage")
lines(summary_data$Time, summary_data$Be, col="royalblue4")
lines(summary_data$Time, summary_data$Pe, col="red4")

par(new=T)
plot(summary_data$Time, summary_data$P, type="l", col="red", xlab="",ylab="", xaxt="n",yaxt="n")
axis(4)
mtext("Lytic phage",side=4,line=3)

par(mar=c(5,4,4,2)+.1)

