survgen2sp <- function(x,y,obs.id,sp.col,sp,o.datetime,s.id,s.start,s.end) {
  #x=df.v # faj adatos
  #y=df # session adatos
  #obs.id="obm_id"
  #sp.col="faj"
  #sp="Vipera ursinii"
  #o.datetime <- "megfigyelesi_datum"
  #s.id='session_id'
  #s.start = 'session_start'
  #s.end = 'session_end'

  x.obs <- unique(na.omit(x[,obs.id]))
  y.obs <- unique(na.omit(y[,obs.id]))

  x.obs.s <- x.obs[!x.obs %in% y.obs]
  x.s <- x[which(x[,obs.id]==x.obs.s),]

  x.s.s <- x.s[which(x.s[,sp.col]==sp),]
  x.s.s[,s.id] <- paste(gsub(" ","",sp),x.s.s[,obs.id],sample(9999,nrow(x.s.s),F),sep="_")

  x.s.s[,s.start] <- x.s.s[,o.datetime]-10*60
  x.s.s[,s.end] <- x.s.s[,o.datetime]+10*60

  return(x.s.s)
}
