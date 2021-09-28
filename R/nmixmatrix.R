nmixmatrix <- function(var.name,id.name,n,x) {
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
