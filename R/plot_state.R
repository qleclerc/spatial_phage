
plot_state = function(play_grid, players_data, turn_num) {
  
  resource_locations = which(play_grid == 1, arr.ind = T)
  
  plot(resource_locations[,"col"], resource_locations[,"row"], type = "p", 
       ylim = c(1, nrow(play_grid)), xlim = c(1, ncol(play_grid)), col = "black", pch = 19,
       main = paste0("Turn ", turn_num), ylab = "", xlab = "", yaxt = "n", xaxt = "n")
  
  points(players_data$x_pos, players_data$y_pos, cex = (players_data$cargo + 1), col = "red", pch = 19)
  
}
