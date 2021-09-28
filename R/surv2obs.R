surv2obs <- function(x,x.date,y.id,y.start,y.end,surveyor) {
  #x = df.o
  #x.date = 'megfigyelesi_datum'
  #y.id='session_id'
  #y.start = 'session_start'
  #y.end = 'session_end'
  #y.dur = 'session_duration
  #min.dur= 5
  #surveyor = 'eszlelo'

  #function
  y <- na.omit(unique(subset(x,select=c(surveyor,y.id,y.start,y.end))))
  x$survey_id <- rep(NA,nrow(x))
  dat <- data.frame()
  surv <- unique(y[,surveyor])
  i <- 1
  for(i in 1:nrow(y)) {
    x.s <- x[x[,surveyor]==surv[i],]
    y.s <- y[y[,surveyor]==surv[i],]
    if (is.na(surv[i])) {
      break
    }
    j <- 1
    y.survs <- unique(y.s[,y.id])
    for(j in 1:length(y.survs )) {
      y.s.s <- y.s[which(y.s[,y.id]==y.survs[j]),]
      x.s.s <- subset(x.s, x.s[,x.date] >= (y.s.s[,y.start]-10*60) & x.s[,x.date] <= (y.s.s[,y.end]+10*60))
      x.s.s$survey_id <- rep(y.survs[j],nrow(x.s.s))
      dat <- rbind(dat,x.s.s)
      print(paste(round(nrow(dat)/nrow(x)*100,1),"%"))
      print(nrow(x.s.s))
      print(y.s[,y.id][j])
    }
  }

  y <- na.omit(unique(subset(dat,select=c(y.id,y.start,y.end))))
  y$survey_id <- y[,y.id]
  dat[,y.id] <- NULL
  dat[,y.start] <- NULL
  dat[,y.end] <- NULL
  dat.y <- merge(dat,y,"survey_id")

  return(dat.y)
}
