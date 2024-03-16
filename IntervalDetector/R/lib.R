largestCommonSubstring=function(words){
  words.split <- strsplit(words, '')
  words.split <- lapply(words.split, `length<-`, max(nchar(words)))
  words.mat <- do.call(rbind, words.split)
  common.substr.length <- which.max(apply(words.mat, 2, function(col) !length(unique(col)) == 1)) - 1
  substr(words[1], 1, common.substr.length)
}

largestCommonPath=function(l){
  l=strsplit(l, "/")
  maxPossible=min(sapply(l, length))
  l=lapply(l, function(x){x[1:maxPossible]})
  l=do.call(rbind.data.frame, l)
  setcolorder(l, rev(colnames(l)))
  if(nrow(unique(l))==1){
    ret=paste(rev(unique(l)), sep="/")
    return(ret)
  }
  while(T){
    if(nrow(unique(l))==1){break;}
    l=unique(l[,-1])
  }
  return(paste(rev(unique(l)), sep="/", collapse="/"))
}
