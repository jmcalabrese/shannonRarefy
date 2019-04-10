#' Generate a rarefaction-type curve based on the Shannon-Weaver diversity index.
#'
#' @param comm A vector with the species ID for each individual in your sample. Each species is represented by a unique integer.
#' @param nrep A number representing the number of random resamplings, without replacement, of \code{comm} from which to generate the rarefaction curve.
#' @return A dataframe with column \code{RAR} representing the rarefaction curve, column \code{SDS} being the standard deviation of the rarefaction curve, and column \code{HOB} representing the Shannon diversity of the full empirical dataset.
shannonRarefy <- function(comm, nrep){
  
  #calculate length of community vector
  lc <- length(comm)
  
  #specify number of rarefaction reps
  #nReps <- 15000
  
  #create an empty data structure to hold the rarefaction reps
  h.rar.rep <- NULL
  
  #loop over reps
  for(i in 1:nrep){
    
    #create a vector of sequential integers from 1 to community sample size to represent positions in the dataset.
    ints <- 1:lc
    
    #create an empty vector to store the (running) sample from the community
    samp <- NULL
    
    #create an empty vector to store the rarefaction curve of the current rep
    h.rar <- NULL
    
    #randomly permute the community. Not strictly necessary, but just to be safe.
    data <- sample(comm, lc, replace=F)
    
    #loop over sample sizes
    for(j in 1:lc){
      
      #sample one index value without replacement
      smp.ind <- sample(ints, 1, replace=F)
      
      #take the individual from the community at the sampled index value
      samp[j] <- data[smp.ind]
      
      #remove that individual from the dataset
      data <- data[-smp.ind]
      
      #remove the sampled individual's index from the index vector
      ints <- ints[-smp.ind]
      
      #calculate and store the Shannon diversity of the current sample.
      h.rar[j] <- shannonDiv(samp)
      
    }
    
    #add the current rarefaction curve to the matrix of all rarefaction curves.
    h.rar.rep <- rbind(h.rar.rep, h.rar)
  }
  
  #return(h.rar.rep)
  
  #calculate the average Shannon diversity of each sample size across all rarefaction curves
  rar <- colMeans(h.rar.rep)
  sds <- apply(h.rar.rep, 2, sd)
  
  #calculate the Shannon diversity of the full empirical dataset
  hob <- shannonDiv(comm)
  
  #create a dataframe for export containing the average rarefaction curve
  #vs sample size, the SD of the rarefaction results vs sample size,
  #and the empirical shannon diversity
  out <- data.frame(RAR=rar, SDS=sds, HOB=hob)
  
  return(out)
  
}