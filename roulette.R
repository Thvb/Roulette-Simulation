####
#Roulette

roulette <- function(startamount,tablelimit,placingstrategy,bettingstrategy,maxloss,maxwin=NULL,maxround=NULL){
 money <- startamount
 placingstrategy <- match.arg(placingstrategy,c("evenbets","random","strat3"))
 stopifnot(any(grepl(c("random|min|max|[0-9]|:"),bettingstrategy)))
 if("martingale"%in%bettingstrategy){
   stopifnot(length(bettingstrategy)==2) 
   stopifnot(sum(grepl(c("[0-9]"),bettingstrategy))==1)
   
 }
 colonMatch <- grepl(":",bettingstrategy)
 if(any(colonMatch)){
   stopifnot(length(bettingstrategy)>=2)
   stopifnot(sum(grepl(c("random|min|max"),bettingstrategy[!colonMatch]))==1)
 }
 betz <- bets()
 wheeloutcomes <- unlist(c(betz$straightbets,betz$zeros))
 betslist <- bets(betslist=T)
 
 payoutslist <- c(rep(35,38),rep(17,50),rep(11,12),rep(5,11),rep(8,22),rep(2,3),rep(2,3),rep(1,2),rep(1,2),rep(1,2))
 names(payoutslist) <- betslist

 
 memo <- mat.or.vec(0,6)
 colnames(memo) <- c("round","bets","roll","payout","money","bankwin")
 
 round <- 1
 stopsim = F
 #x11()
 if(is.null(maxround)){
  logic <- (money<(startamount+maxwin))&(money>(startamount-maxloss))&(money>0)&(min(tablelimit)<money)&stopsim==F
 }else{
   if(is.null(maxwin)){
     logic <- (round<=maxround)&(money>(startamount-maxloss))&(money>0)&(min(tablelimit)<money)&stopsim==F
   }else{
     logic <- (money<(startamount+maxwin))&(money>(startamount-maxloss))&(money>0)&(min(tablelimit)<money)&(round<=maxround)&stopsim==F
   }
 }
 numberlosses <- 0
 numberwins <- 0
 lastbet <- 0
 while(logic){
   
   playerbets <- player(money=money,lastbet=lastbet,numberwins=numberwins,numberloss=numberlosses,placingstrategy=placingstrategy,bettingstrategy=bettingstrategy,round=round,tablelimit=tablelimit,bets=betslist,payouts=payoutslist)
   lastbet <- playerbets
   if(!any(grepl("Simulation stopped",playerbets))){
   roll <- wheel(wheeloutcomes)
  
   payout <- outcome(roll=roll,bet=playerbets,allbets=betslist,betdetails=betz,payoutlist=payoutslist)
   
   if(payout<0){
     numberlosses <- numberlosses+1
     numberwins <- 0
   }else{
     numberlosses <- 0
     numberwins <- numberwins+1
   }
   money <- money+payout
   bankwin <- startamount-money
   
   }else{
     stopsim = T
   }
   memo <- rbind(memo,c(round,paste(playerbets,collapse="|"),roll,payout,money,bankwin))
   
   round <- round+1
   #barplot(c(money,payout))
   if(is.null(maxround)){
     logic <- (money<(startamount+maxwin))&(money>(startamount-maxloss))&(money>0)&(min(tablelimit)<money)&stopsim==F
   }else{
     if(is.null(maxwin)){
       logic <- (round<=maxround)&(money>(startamount-maxloss))&(money>0)&(min(tablelimit)<money)&stopsim==F
     }else{
       logic <- (money<(startamount+maxwin))&(money>(startamount-maxloss))&(money>0)&(min(tablelimit)<money)&(round<=maxround)&stopsim==F
     }
   }
 }
 
 memo <- data.frame(memo)
 for(i in c(1,4:6)){
  memo[,c(i)]<- as.numeric(as.character(memo[,i]))
 }
 rm(bettingsequence,envir=environment(roulette))
 return(memo)

 }