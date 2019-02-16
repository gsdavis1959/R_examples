library(MASS)       # load the MASS package
data("survey")
head(survey)
tbl = table(survey$Smoke, survey$Exer) 
tbl                 # the contingency table 

chisq.test(tbl)

challenge.df = matrix(c(1,4,7,4), nrow = 2)
challenge.df

heads <- rbinom(1, size = 100, prob = .5)
prop.test(heads, 100)          # continuity correction TRUE by default
prop.test(heads, 100, correct = FALSE)

## Data from Fleiss (1981), p. 139.
## H0: The null hypothesis is that the four populations from which
##     the patients were drawn have the same true proportion of smokers.
## A:  The alternative is that this proportion is different in at
##     least one of the populations.

groups  <- c( 8, 6, 12)
group_sample <- c( 100, 100, 100)
prop.test(groups, group_sample)

