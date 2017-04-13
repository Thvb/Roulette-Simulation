####
#Bets
bets <- function(betslist=F){
straightbets <- matrix(c("1R","2B","3R","4B","5R","6B","7R","8B","9R","10B","11B","12R","13B","14R","15B","16R","17B","18R","19R","20B","21R","22B","23R","24B","25R","26B","27R","28B","29B","30R","31B","32R","33B","34R","35B","36R"),1,36)
colnames(straightbets) <- paste("straightbets",1:ncol(straightbets))
zeros <- matrix(c("0","00"),1,2)
colnames(zeros) <- paste("zeros",1:ncol(zeros))
splitbets <- matrix(c("1R","2B","1R","4B","2B","5R","2B","3R","3R","6B","4B","5R","4B","7R","5R","8B","5R","6B","6B","9R","7R","10B","7R","8B","8B","11B","8B","9R","9R","12R","10B","13B","10B","11B","11B","14R","11B","12R","12R","15B","13B","14R","13B","16R","14R","17B","14R","15B","15B","18R","16R","19R","16R","17B","17B","20B","17B","18R","18R","21R","19R","22B","19R","20B","20B","23R","20B","21R","21R","24B","25R","28B","25R","26B","26B","29B","26B","27R","27R","30R","28B","31B","28B","29B","29B","32R","29B","30R","30R","33B","31B","34R","31B","32R","32R","35B","32R","33B","33B","36R"),nrow=2,ncol=50)
colnames(splitbets) <- paste("splitbets",1:ncol(splitbets))


streetbets <- matrix("",ncol=length(straightbets)/3,nrow=3)
sequence <- seq(1,length(straightbets),by=3)
for(i in 1:ncol(streetbets)){
  q <- c(straightbets[sequence[i]:(sequence[i]+2)])
  streetbets[,i] <- q
}

colnames(streetbets) <- paste("streetbets",1:ncol(streetbets))

linebets <- matrix("",ncol=11,nrow=6)
for(i in 1:ncol(linebets)){
  q <- c(streetbets[,i],streetbets[,i+1])
  linebets[,i] <-q
}
colnames(linebets) <- paste("linebets",1:ncol(linebets))

sequence1 <- seq(1,31,by=3)
sequence2 <- seq(2,32,by=3)
sequence <- sort(c(sequence1,sequence2))
cornerbets <- matrix("",ncol=length(sequence),nrow=4)
for(i in 1:length(sequence)){
  q<-c(straightbets[sequence[i]],straightbets[sequence[i]+1],straightbets[sequence[i]+3],straightbets[sequence[i]+4])
  cornerbets[,i] <- q
}

colnames(cornerbets) <- paste("cornerbets",1:ncol(cornerbets))


endoflayout <- c("0","1R","2B","3R","00")

#outsidebets

col1 <- c(straightbets[seq(1,34,by=3)])
col2 <- c(straightbets[seq(2,35,by=3)])
col3 <- c(straightbets[seq(3,36,by=3)])

First12 <- c(straightbets[1:12])
Second12 <- c(straightbets[13:24])
Third12 <- c(straightbets[25:36])

Low118 <- c(straightbets[1:18])
High1936 <- c(straightbets[19:36])

Even <- c(straightbets[seq(2,36,by=2)])
Odd <- c(straightbets[seq(1,35,by=2)])

redseq <- c(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)
blackseq <- (1:36)
blackseq <- blackseq[-redseq]
Red <- c(straightbets[redseq])
Black <- c(straightbets[blackseq])

ans <- list()
ans$zeros <- zeros
ans$straightbets <- straightbets
ans$splitbets <- splitbets
ans$streetbets <- streetbets
ans$linebets <- linebets
ans$cornerbets <- cornerbets
ans$first12 <- First12
ans$second12 <- Second12
ans$third12 <- Third12
ans$col1 <- col1
ans$col2 <- col2
ans$col3 <- col3
ans$low118 <- Low118
ans$high1936 <- High1936
ans$even <- Even
ans$odd <- Odd
ans$Red <- Red
ans$Black <- Black



if(betslist==F){
  return(ans)
}else{
  return(c(unlist(lapply(ans[1:6],function(x){if(is.matrix(x)){colnames(x)}})),paste0(names(ans)[7:length(ans)])))
}

}