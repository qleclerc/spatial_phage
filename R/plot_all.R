#' @title Plots the bacteria and phage
#'
#' @description Plots the current position and status of the bacteria and phage
#'
#' @param data_bac The dataset containing the bacteria information
#' @param data_pha The dataset containing the phage information
#' @param size The size of the space within which to place the bacteria and phage. Must be the same as the one used to place them.
#'
#' @return A plot
#'
#' @examples
#' plot_all(data=dots_data, size=100)
#'
#' @export

plot_all = function(data_bac,data_pha,size=100,legnd=T,title=NULL){

  subdata = data_bac[data_bac$status=="S",]
  subdata2 = data_bac[data_bac$status=="E",]
  subdatam = data_bac[data_bac$status=="M",]


  subdata3 = data_pha[data_pha$status=="P",]
  subdata4 = data_pha[data_pha$status=="E",]

  plot(main=title,x=subdata$x,y=subdata$y,pch=19,col="royalblue1",ylim=c(0,size),xlim=c(0,size),xaxt="n",yaxt="n",xlab="",ylab="")
  #text(data$y~data$x,labels=as.character(data$id),pos=3,cex=0.7)
  points(subdata2$x,subdata2$y,pch=19,col="royalblue4")

  points(subdata3$x,subdata3$y,pch=18,col="red")
  points(subdata4$x,subdata4$y,pch=18,col="green")
  points(subdatam$x,subdatam$y,pch=19,col="magenta",cex=2)

  if(legnd) legend("topright", legend=c("S bac","E bac","P pha","E pha"), col=c("royalblue1","royalblue4","red","red4"), pch=c(19,19,18,18))

}
