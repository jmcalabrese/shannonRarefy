#' Calculate the Shannon-Weaver diversity index
#'
#' @param comm A vector with the species ID for each individual in your sample. Each species is represented by a unique integer.
#' @return A number representing the Shannon diversity of the input sample.
shannonDiv <- function(comm){
  
  #tally up the total number of individuals of each spp in the current sample.
  counts <- as.data.frame(table(comm))
  
  #calculate the total number of individuals across all spp in the current sample.
  tot <- sum(counts$Freq)
  
  #calculate the proportional abundance of each spp in the current sample.
  p.abunds <- counts$Freq / tot
  
  #calculate and store the Shannon diversity of the current sample.
  h.rar <- -sum(p.abunds * log(p.abunds))
  
  return(h.rar)
  
}