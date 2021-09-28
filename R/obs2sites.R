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
