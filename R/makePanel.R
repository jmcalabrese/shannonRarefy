#A helper function for rarePlot. Not exported to the namespace.
makePanel <- function(rar, sds, hob, let){
  
  #calculate x and y plot limits from input data
  xmax <- length(rar)-1
  ymax <- max(rar+sds) + 0.1*max(rar+sds)
  
  #create the core plot
  plot(rar, ylab="", xlab="", 
       xlim=c(0, xmax), ylim=c(0, ymax), type='l', col='white',
       cex.axis=1.25)
  
  #create and plot the error bars
  x <- 1:length(rar)
  arrows(x, rar-sds, x, rar+sds, length=0.01, angle=90, 
         code=3, col="gray75")
  
  #plot the avg rarefaction curve and empirical shannon div
  lines(rar, lwd=1.5)
  lines(hob, col='red', lwd=1.5)
  
  #add the panel label
  text(xmax - 0.1*xmax, 0.1*ymax, paste0(let, ")"), cex=1.25)
  
}