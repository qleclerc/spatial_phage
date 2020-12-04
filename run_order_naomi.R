library(animation)
#E = resistant
#S = susceptible
#p

p = set_params(size=100,rep_bac=0.5,rep_bac_res=0.2,movement_bac=1,
               abx_con = 0, mutate_bac = 0.01)

#size = initial pop size, will increase to grid size (10xsize)
#rep__bac = reproduction rate
#movement = - to +, limited to steps of integers
#abx_con = currently not defined, up to me to decide how this interacts with bacteria,
# can lit on this topic, try to make a function for this, can start with rep_bac as draft
#mutate_bac = mutation rate, time units are abstract

bac = create_bac(50, prop_res = 0.5) #create bacteria
pha = create_pha(0) #must be zero to not include phage

bac = place_any(bac, p["size"]) #place bacteria on grid, size = size of grid

plot_all(bac, pha, p["size"], legend = T) #check it's working

times = 50 #amount of time simulations run, there is a limit, maybe around 100

summary_data = data.frame(Time=c(0:times), Bs=0, Be=0) #store at each timestep no of bacteria and positions
summary_data[1,"Bs"] = length(which(bac$status=="S"))
summary_data[1,"Be"] = length(which(bac$status=="E"))

saveGIF({

  for(t in 2:(times+1)){

    message("Round ", t-1, " out of ", times)
    #bac = abx_kill_bac(bac, p["abx_con"])
    bac = rep_bac(bac, p["size"], p["rep_bac"], p["rep_bac_res"]) #replicates the bacteria
    bac = mutate_bac(bac, p["mutate_bac"]) #mutating s to r
    bac = move_any(bac, p["size"], p["movement_bac"])

    title = paste0("% of antibiotic resistant bacteria: ",
                   round(length(which(bac$status == "E"))/dim(bac)[1]*100)) #title changes with each timestep, percentage of resistant bacteria
    plot_all(bac, pha, p["size"],F,title=title)

    summary_data[t,"Bs"] = length(which(bac$status=="S")) #need to record data otherwise it will be overwriiten
    summary_data[t,"Be"] = length(which(bac$status=="E"))
  }
}, movie.name = "test.gif", interval=0.01)


plot(summary_data$Time, summary_data$Bs, type="l", col="lightblue3",
     ylim=c(0,p["size"]*10), xlab="Time", ylab="Bacteria")
lines(summary_data$Time, summary_data$Be, col="royalblue4")


