obssums2surv <- function(x,y,session.id,site.id,sp.col,spp) {
  #x=df
  #y=surv
  #session.id="session_id"
  #site.id ="ID_SHORT"
  #sp.col="faj"
  #spp = c("Lacerta agilis","Lacerta viridis","Vipera ursinii")

  x <- x[which(!is.na(x[,sp.col])),]
  x$comb <- paste(x[,site.id],x[,session.id])
  y$comb <- paste(y[,site.id],y[,session.id])

  y.s <- y

  i <- 1
  for(i in 1:length(spp)) {
    x.sp <- x[which(x[,sp.col]==spp[i]),]
    x.sp$n <- 1
    x.sp.sum <- aggregate(n~comb,x.sp,sum)
    colnames(x.sp.sum)[2] <- spp[i]
    y.s <- merge(y.s,x.sp.sum,all.x=T)
    print(spp[i])
  }
  y.s[,(ncol(y)+1):ncol(y.s)][is.na(y.s[,(ncol(y)+1):ncol(y.s)])] <- 0

  return(y.s)
}
