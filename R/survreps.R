survreps <- function(x,sp,season,site.id,reps) {
  x=surv
  #sp="Vipera ursinii"
  season="season"
  #site.id="ID_SHORT"
  #reps=20

  dat <- data.frame()
  seasons <- unique(x[,season])

  i <- 1
  for(i in 1:length(seasons)) {
    x.s <- x[which(x[,season]==seasons[i]),]

    dat.s <- data.frame()
    ids <- unique(x.s[,site.id])
    j <- 1
    for(j in 1:length(ids)) {
      x.s.i <- x.s[which(x.s[,site.id]==ids[j]),]
      nrow(x.s.i)
      if(nrow(x.s.i)>=(reps+1)) {
        x.s.i.pres <- x.s.i[which(x.s.i[,sp]>=1),]
        x.s.i.abs <- x.s.i[which(x.s.i[,sp]==0),]
        x.s.i.abs <- x.s.i.abs[sample.int(nrow(x.s.i.abs),reps-nrow(x.s.i.pres),replace=F),]
        x.s.i <- rbind(x.s.i.pres,x.s.i.abs)
      }
      if(nrow(x.s.i)<=reps) {
        x.s.i <- x.s.i[sample.int(nrow(x.s.i),reps,replace=T),]
      }
      dat.s <- rbind(dat.s,x.s.i)
    }
    dat <- rbind(dat,dat.s)
  }
  return(dat)
} ##beépíteni egy repeat-et!! érje el az eredeti sum-ot a replikatumok
