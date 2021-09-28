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
