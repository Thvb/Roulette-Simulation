###
#Outcome class

outcome <- function(roll,bet,allbets,betdetails,payoutlist){
  
  stopifnot(nrow(bet)==2)
  betnames <- match.arg(bet[1,],allbets,several.ok=T)
  betamount <- as.numeric(bet[2,])
  names(betamount) <- betnames
  
  #outsidebets <- c("col1","col2","col3","First12","Second12","Third12","Low1-18","High19-36","Even","Red","Black","Odd")
  
  betsoutlined <- vector(length(betnames),mode="list")
  for(i in 1:length(betnames)){
    searchname <- gsub(" .*$", "", betnames[i])
    searchname <- gsub(" ", "", searchname)
    betmatching <- match(searchname,names(betdetails))
    
    tmp <- betdetails[[betmatching]]
    if(is.matrix(tmp)){
      betsoutlined[[i]] <- tmp[,colnames(tmp)%in%betnames[i]]
    }else{

      betsoutlined[[i]] <- tmp
    }
  }
  names(betsoutlined) <- betnames
  
  
  didwin <- unlist(lapply(betsoutlined,function(x){roll%in%x}))
  if(all(!didwin)){
    totaloutcome <- -sum(betamount)
  }else{
    if(all(didwin)){
      totaloutcome <- (payoutlist[match(bet[1,didwin],names(payoutlist))]*betamount[didwin])
    }else{
      totaloutcome <- (payoutlist[match(bet[1,didwin],names(payoutlist))]*betamount[didwin])-sum(betamount[!didwin])
    }
  }

return(totaloutcome)
}