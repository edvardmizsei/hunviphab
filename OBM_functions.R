#OBM functions

wkt2coords <- function(data,geometry,input.crs,output.crs) {
  geom <- sub(geometry, pattern = "POINT", replacement = "")
  geom <- sub(geom, pattern = "[(]", replacement = "")
  geom <- sub(geom, pattern = "[)]", replacement = "")
  xy <- unlist(strsplit(geom, split = " "))
  temp.coord <- cbind(as.numeric(xy[seq(1, length(xy), 2)]),as.numeric(xy[seq(2, length(xy), 2)]))
  colnames(temp.coord) <- c("x","y")
  
  require(sf)
  temp.coord <- as.data.frame(st_transform(st_as_sf(as.data.frame(temp.coord), coords=c("x","y"),crs=input.crs),crs=output.crs))
  
  geom <- sub(temp.coord$geometry,pattern="c",replacement="")
  geom <- sub(geom, pattern = "[(]", replacement = "")
  geom <- sub(geom, pattern = "[)]", replacement = "")
  xy <- unlist(strsplit(geom, split = ", "))
  temp.coord <- cbind(as.numeric(xy[seq(1, length(xy), 2)]),as.numeric(xy[seq(2, length(xy), 2)]))
  colnames(temp.coord) <- c("x","y")
  return(temp.coord)
}

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

n.mix.matrix <- function(var.name,id.name,n,x) {
  #var.name= "Vipera ursinii"
  #id.name="ID_SHORT"
  #n= "n"
  #x=df.r
  
  x$column.count <- paste(var.name,x[,n],sep="_")
  column.count <- unique(x$column.count)
  rows <- unique(x[,id.name])
  var.values <- data.frame(array(0,c(length(rows),length(column.count))))
  colnames(var.values) <- column.count
  rownames(var.values) <- rows
  i <- 1
  for(i in 1:nrow(x)) var.values[which(rows==x[,id.name][i]),which(column.count==x$column.count[i])] <- x[,var.name][i]
  return(var.values)
  
}

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

obs2sites <- function(x,x.coord,y.coord,x.season.col,sites,sites.col,seasons,crs) {
  #x = df
  #x.coord = "x"
  #y.coord = "y"
  #x.season.col = "season"
  #sites = k
  #sites.col="season"
  #seasons = c("20_tavasz","20_osz","21_tavasz","21_osz")
  #crs = 23700
  require(sf)
  
  i <- 1
  dat <- data.frame()
  for(i in 1:length(seasons)) {
    x.s <- x[which(x[,x.season.col]==seasons[i]),]
    sites.s <- sites[which(sites$season==seasons[i]),]
    
    x.s <- st_as_sf(x.s, coords=c(x.coord,y.coord),crs=crs)
    
    x.s_int <- st_contains_properly(sites.s,x.s)
    x.s <- x.s[c(unlist(x.s_int)),]
    x.s <- st_join(x.s,sites.s)
    
    x.s <- unique(x.s)
    dat <- rbind(dat,x.s)
    print(paste(seasons[i],nrow(x.s)))
  }
  return(dat)
}

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

##beépíteni egy repeat-et!! érje el az eredeti sum-ot a replikatumok

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
}

rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- NA
  
  y[, c(as.character(x.diff))] <- NA
  
  return(rbind(x, y))
} # nem saját

se <- function(x, ...) sqrt(var(x, ...)/length(x)) # nem saját

