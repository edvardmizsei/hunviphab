survreps_missing <- function(x,sp,season,site.id,reps,missingness) {
  #x=surv
  #sp="Vipera ursinii"
  #season="season"
  #site.id="ID_SHORT"
  #reps=20
  #missingness=0.25

  dat <- data.frame()
  seasons <- unique(x[,season])

  i <- 1
  for(i in 1:length(seasons)) {
    x.s <- x[which(x[,season]==seasons[i]),]
    repeat{
      dat.s <- data.frame()
      ids <- unique(x.s[,site.id])
      j <- 1
      for(j in 1:length(ids)) {
        x.s.i <- x.s[which(x.s[,site.id]==ids[j]),]
        nrow(x.s.i)
        if(nrow(x.s.i)>=(reps+1)) {
          x.s.i.pres <- x.s.i[which(x.s.i[,sp]>=1),]
          if(nrow(x.s.i.pres)>=(reps+1)) {
            x.s.i <- x.s.i.pres[sample.int(nrow(x.s.i.pres),reps,replace=F),]
          }
          if(nrow(x.s.i.pres)<=(reps+1)) {
            x.s.i.abs <- x.s.i[which(x.s.i[,sp]==0),]
            x.s.i.abs <- x.s.i.abs[sample.int(nrow(x.s.i.abs),reps-nrow(x.s.i.pres),replace=F),]
            x.s.i <- rbind(x.s.i.pres,x.s.i.abs)
          }
        }
        if(nrow(x.s.i)>=floor(reps-reps*missingness)) {
          x.s.i <- x.s.i
        }
        if(nrow(x.s.i)<=(floor(reps-reps*missingness)-1)) {
          x.s.i <- x.s.i[sample.int(nrow(x.s.i),reps,replace=T),]
        }
        dat.s <- rbind(dat.s,x.s.i)
      }
      if(sum(dat.s[,sp])>=sum(x.s[,sp])) {
        break
      }
    }
    dat <- rbind(dat,dat.s)
  }
  return(dat)
}
