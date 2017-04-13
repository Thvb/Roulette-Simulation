###
#Player

player <- function(money,lastbet,numberwins,numberloss,placingstrategy,bettingstrategy,round,tablelimit,bets,payouts){
  stopifnot(length(tablelimit)<3)
  if(length(tablelimit)==1){
    tablemin = 1
    tablemax = tablelimit
  }else{
    tablemin = min(tablelimit)
    tablemax = max(tablelimit)
  }
  
  minbet <- tablemin
  
  if(money<=tablemax){
    maxbet <- money
  }else{
    maxbet <- tablemax
  }
  
  numMatch <- grepl("\\d[0-100000]",bettingstrategy,fixed=F)
  colonMatch <- grepl(":",bettingstrategy)
  if(any(numMatch)&all(colonMatch==F)){
    stopifnot(sum(numMatch)==1)
    betpile <- as.numeric(bettingstrategy[numMatch])
  }
  if(any(colonMatch)){
    stopifnot(sum(colonMatch)==1)
    range <- as.numeric(unlist(strsplit(bettingstrategy[numMatch],":")))
    if(min(range)>tablemin){
    minbet <- min(range)
    }
    maxbet <- max(range)
  }
  if("random"%in%bettingstrategy){
    betpile <- sample(minbet:maxbet,1)
  }
  if("min"%in%bettingstrategy){
    betpile <- minbet
  }
  if("max"%in%bettingstrategy){
    betpile <- maxbet
  }
  if("fives"%in%bettingstrategy){
    betpile <- sample(seq(0,maxbet,by=5),1)
  }
  if(("martingale"%in%bettingstrategy)){
    betpile <- betpile*(2^numberloss)
    if(betpile>tablemax){
      playerbets <- c(paste("Simulation stopped: Martingale bet of",betpile,"supersedes the tablelimit of",paste0(tablemax,". No more bets allowed.")))
      return(playerbets)
    }
    numberbets <- 1
    placingstrategy <- "evenbets"
  }
  if("1326multip."%in%bettingstrategy){
      numberwins <- numberwins+1
      nw <- rep(c(1,3,2,6),length.out=numberwins)
      nw <- nw[numberwins]
      betpile <- betpile*nw
  }
  if("cancellation"%in%bettingstrategy){
    if(!exists("bettingsequence")){
      assign("bettingsequence",seq(1,(betpile-1),by=1),envir=environment(roulette),inherits=T)
    }
      betseq <- get("bettingsequence",envir=environment(roulette))
    if(numberwins>0){
      betseq <- bettingsequence[-c(1,length(bettingsequence))]
      assign("bettingsequence",betseq,envir=environment(roulette),inherits=F)
    }
    if(numberloss>0){
      betseq <- c(bettingsequence,as.numeric(lastbet[2]))
      assign("bettingsequence",betseq,envir=environment(roulette),inherits=F)
    }
  if(length(betseq)==0){
    assign("bettingsequence",seq(1,(betpile-1),by=1),envir=environment(roulette),inherits=T)
    betseq <- get("bettingsequence",envir=environment(roulette))
  }
  
    betpile <- betseq[c(1)]+betseq[length(betseq)]
  if(betpile>maxbet){
    playerbets <- c(paste("Simulation stopped: Cancellation bet of",betpile,"supersedes the money of the player."))
    return(playerbets)
  }
  if(betpile>tablemax){
    playerbets <- c(paste("Simulation stopped: Cancellation bet of",betpile,"supersedes the tablelimit of",paste0(tablemax,". No more bets allowed.")))
    return(playerbets)
  }
  }
  
  evenbets <- bets[payouts==1]
  numberbets <- sample(1,1)
  
  ok <- F
  while(ok==F){
    if(placingstrategy=="random"){
      bettypes <- sample(bets,numberbets)
    }
    if(placingstrategy=="evenbets"){
      bettypes <- sample(evenbets,numberbets)
    }
    if((betpile%%length(bettypes))==0){
      ok<-T
    }
  }
  
  
  playerbets <- rbind(bettypes,rep(1,length(bettypes)))
  betpile <- betpile - length(bettypes)
  if(betpile>0){
    while(betpile>0){
      amount <- sample(1:betpile,1)
      type <- sample(1:ncol(playerbets),1)
      
      playerbets[2,type] <- as.numeric(playerbets[2,type])+amount
      betpile <- betpile-amount
    }
  }
  rownames(playerbets) <- c("bettypes","betamounts")
  return(playerbets)
}

