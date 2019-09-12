library(devtools)
install_github("dustinfife/fifer")
library(RVAideMemoire)


# Makes a table of observations -- similar to first example in chisq.test
M <- as.table(rbind(c(76, 32, 46), c(48,23,47), c(45,34,78)))
dimnames(M) <- list(sex=c("Male","Female","Juv"),loc=c("Lower","Middle","Upper"))
M
# Shows post-hoc pairwise comparisons using fdr method
chisq.multcomp(M, p.method = "bonferroni")
