#' Make a 3-panel plot with rarefaction curves from different samples. This is not currently intended for general usage and is tailored to the paper this package supports.
#'
#' @param panA An object from the \code{shannonRarefy()} function representing the rarefaction curve of the first sample.
#' @param panB An object from the \code{shannonRarefy()} function representing the rarefaction curve of the second sample.
#' @param panC An object from the \code{shannonRarefy()} function representing the rarefaction curve of the third sample.
#' @param plotname The filename given to the output plot. Defaults to \code{"rarefaction.pdf"}.
#' @param dir The location of the directory to which the output plot should be written. Defaults to current working directory.
#' @return A three-panel plot, written to disk as a .pdf file
rarePlot <- function(panA, panB, panC, plotname="rarefaction.pdf", path="."){

  #set the output directory. This defaults to the current working directory
  setwd(path)

  #open a pdf device
  pdf(file=plotname, height=7, width=3.5)

  #specify layout parameters
  #this is currently hard-coded to produce the figure in the paper
  par(mfrow=c(3,1),
      oma = c(3, 3, 0.2, 0.2), mar = c(2, 2, 1, 1))

  #add the individual panel plots
  makePanel(panA$RAR, panA$SDS, panA$HOB, "A")
  makePanel(panB$RAR, panB$SDS, panB$HOB, "B")
  makePanel(panC$RAR, panC$SDS, panC$HOB, "C")

  #add x and y axis labels to the multipanel plot
  mtext("Sample size", side = 1, outer = TRUE, line = 1.5,
        cex=1.2)
  mtext("Shannon diversity", side = 2, outer = TRUE, line = 1.5,
        cex=1.2)

  #close the pdf device to write the plot to the specified directory
  dev.off()

}
