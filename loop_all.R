
seed_num = runif(1, 0, 1000)
set.seed(seed_num)

source("run_order.R")

prop_r = tail(summary_data$Be, 1)/(tail(summary_data$Bs, 1) + tail(summary_data$Be, 1))*100

if(is.nan(prop_r)){
  source("loop_all.R")
} else if(prop_r > 70){
  beepr::beep(5)
  print(seed_num)
} else {
  source("loop_all.R")
}
