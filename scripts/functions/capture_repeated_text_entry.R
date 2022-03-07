#' Function to capture repeated text entries in text variables. Returns a dataframe with a list of text chunks and the number of times each chunk is counted, ordered from most to fewest counts.
#'
#' @return Dataframe with a list of text chunks and the number of times each chunk is counted, ordered from most to fewest counts.
#' 
#' @param str Vector of strings to evaluate
#' @param len Length of text chunks to consider as repeated entries in number of characters
#' 
#' @examples
#' # Capture text entries of length 3 for each measurement and all variables in the dataframe 'test'.
#' #test<-data.frame(id=c("A","B","C"), v=c("1761176","182","117517552"))
#' #textEntries<-capture_repeated_text_entry(str = test$v, len = 3) #get a list of dataframes with occurring text chunks of specified length with its count for each measurement, sorted in decreasing order of counts.
#' 
#' # Extract the first occurring text entry based on the number of counts for each measurement
#' #lapply(X = textEntries,FUN = function(x){x[[1,1]]})
#' 
#' @export
#'
capture_repeated_text_entry<-function(str, len=2){
  internalCaptureRepeatedTextEntry<-function(str, len=2){
    parts<-vector()
    for(nStr in 1:(nchar(str)-len+1)){
      parts<-cbind(parts,substr(str,start = nStr,stop = (nStr+len-1)))
    }
    
    parts<-as.vector(parts)
    uparts<- unique(parts)
    counts<-vector()
    for(nUpart in 1:length(uparts)) counts[nUpart]<-sum(uparts[nUpart] == parts)
    
    toreturn<-data.frame(parts=uparts, counts=counts)
    toreturn$parts<-as.character(toreturn$parts)
    toreturn<-toreturn[order(toreturn$counts, decreasing = T),]
    
    return(toreturn)
  }
  
  return(lapply(X = str, FUN = internalCaptureRepeatedTextEntry, len =len))
}