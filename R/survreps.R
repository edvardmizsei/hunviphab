survreps <- function(x,sp,season,site.id,reps) {
  #x=surv
  #sp="Vipera ursinii"
  #season="season"
  #site.id="ID_SHORT"
  #reps=20

  # for seasons i
seasons <- unique(x[,season])
dat <- data.frame()
i <- 1
for(i in 1:length(seasons)) {
  x.i <- x[which(x[,season]==seasons[i]),]
  
  ## for sites j
  ids <- unique(x.i[,site.id])
  dat.j <- data.frame()
  j <- 1
  for(j in 1:length(ids)) {
    x.j <- x.i[which(x.i[,site.id]==ids[j]),]
    
    ### if surevs MORE than needed reps
    if(nrow(x.j)>reps) {
      x.j.pres <- x.j[which(x.j[,sp]>=1),]
      
      #### if surveys with obs zero
      if(nrow(x.j.pres)==0) {
        x.j.sel <- x.j[sample.int(nrow(x.j),reps,replace=F),]
      }
      
      #### if surveys with obs between 1 & reps
      if(nrow(x.j.pres)>0 & nrow(x.j.pres)<=reps) {
        x.j.abs <- x.j[which(x.j[,sp]==0),]
        x.j.abs <- x.j.abs[sample.int(nrow(x.j.abs),reps-nrow(x.j.pres),replace=F),]
        x.j.sel <- rbind(x.j.pres,x.j.abs)
      }
      
      #### if survey with obs == n reps
      if(nrow(x.j.pres)==reps) {
        x.j.sel <- x.j.pres
      }
      
      #### if survey with obs more than reps
      if(nrow(x.j.pres)>reps) {
        x.j.sel <- x.j.pres[sample.int(nrow(x.j.pres),reps,replace=F),]
      }
    }
    
    ### if surveys LESS than needed reps
    if(nrow(x.j)<=reps) {
      x.j.sel <- x.j[sample.int(nrow(x.j),reps,replace=T),]
    }
    dat <- rbind(dat,x.j.sel)
  }
}
  return(dat)
}


