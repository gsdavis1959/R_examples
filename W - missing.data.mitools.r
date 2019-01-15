library(mitools)

data(smi)
smi
models<-with(smi, glm(drinkreg~wave*sex,family=binomial()))
summary(MIcombine(models))

betas<-MIextract(models,fun=coef)
vars<-MIextract(models, fun=vcov)
summary(MIcombine(betas,vars))

# replace with mean
df = data.frame(x = 1:20, y = c(1:10,rep(NA,10)))
df
df$y[is.na(df$y)] = mean(df$y, na.rm=TRUE)
# alternative
df = transform(df, y = ifelse(is.na(y), mean(y, na.rm=TRUE), y))
df
