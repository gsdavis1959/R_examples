datdum <- function(x, data, name){
  data$rv <- rnorm(dim(data)[1],1,1)
  mm <- data.frame(model.matrix(lm(data$rv~-1+factor(data[,x]))))
  names(mm) <- paste(name,1:dim(mm)[2],sep=".")
  data$rv <- NULL
  data <- cbind(data,mm)
  return(data)
}

# simple example
dat <- c("A","B","C")
dat <- data.frame(dat)
datdum(x="dat",data=dat,name="category")
#########################
# output
#########################
dat

datdum(x="dat",data=dat,name="category")
# alternative function
datdum <- function(x, data, name=x){
  mm <- data.frame(contr.treatment(length(levels(factor(data[,x]))), contrasts=FALSE))
  names(mm) <- paste(name,1:ncol(mm),sep=".")
  data <- cbind(data,mm)
  return(data)
}
