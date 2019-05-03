dte = seq(as.Date("2011-05-01"), as.Date("2011-05-20"), by=1)
persid = c(1013,1011,1014,1015,1023,1028,1012,1018,1019,1020,1027,
           1016,1022,1017,1021,1024,1030,1025,1026,1029)
v1 = round(rnorm(20), 2)
v2 = round(rnorm(20), 2)
first=c("David","Sara","Jon","Jennifer","Ken","Ralph","Chris","David",
        "David","Joe","Melanie","Debbie","Jessica","Ally","Amy","Ralph",
        "Sara","Jane","John","Lance")
last=c("Smith","Jones","Alberts","Hudson","Jennings","Masterson","Browm",
       "Felt","Spade","Montana","Keith","Hardson","Karson","Roberts","Smith",
       "Jennings","Denver","Hudson","Reynolds","Darder")
stat = c("CA","IA","NC","FL","GA","OH","NY","CA","TX","TX","CA","CA","AZ",
         "CO","OK","MI","WI","SC","VT","IL")

df1 <- data.frame(id=c(seq(1,20)), date=c(dte), var1=c(v1),
                  var2=c(v2), personid=c(persid))

df2 <- data.frame(id=c(sort(persid)), firstname=c(first),
                  lastname=c(last), state=c(stat))
library(sqldf)

sqldf("SELECT COUNT(*) FROM df2 WHERE state = 'CA'")

sqldf("SELECT df2.firstname, df2.lastname, df1.var1, df2.state FROM df1
INNER JOIN df2 ON df1.personid = df2.id WHERE df2.state = 'TX'")
sqldf("SELECT df2.state, COUNT(df1.var1) FROM df1
INNER JOIN df2 ON df1.personid = df2.id WHERE df1.var1 > 0
GROUP BY df2.state")
sqldf("SELECT df2.firstname, df2.lastname, df1.var1, df2.state FROM df1
INNER JOIN df2 ON df1.personid = df2.id
WHERE df1.date BETWEEN '2011-05-03' AND '2011-05-11'")
