source("run_order.R")

prop_r = summary_data$Be[201]/(summary_data$Bs[201]+summary_data$Be[201])*100

if(is.nan(prop_r)){
  assign("seed_num", seed_num+1, envir=.GlobalEnv)
  source("loop_all.R")
} else if(prop_r > 70){
  beepr::beep(5)
  print(seed_num)
} else {
  assign("seed_num", seed_num+1, envir=.GlobalEnv)
  source("loop_all.R")
}
