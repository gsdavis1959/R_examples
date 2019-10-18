library(afex)
data("fhch2010") # load data (comes with afex) 

mean(!fhch2010$correct) # error rate
# [1] 0.01981546
fhch <- droplevels(fhch2010[ fhch2010$correct,]) # remove errors

str(fhch2010) # structure of the data

(a1 <- aov_car(log_rt ~ task*length*stimulus + Error(id/(length*stimulus)), fhch))
# Contrasts set to contr.sum for the following variables: task
# Anova Table (Type 3 tests)
# The standard output also reports Greenhouse-Geisser (GG) 
# corrected df for repeated-measures factors with more than two levels
# ges which provides generalized eta-squared,
# ‘the recommended effect size statistics for repeated measure design
# It must have an Error term specifying the column containing the participant
# (or unit of observation) identifier (e.g., minimally +Error(id)). 
# This is necessary to allow the automatic aggregation even in designs without
# repeated-measures factor.

# alternative format
a2 <- aov_ez(id = "id", dv = "log_rt", fhch, between = "task", within = c("length", "stimulus"))
library(lsmeans)
# follow up tests
emmeans(a2, c("stimulus","task"))

# set up conditional marginal means:
(ls1 <- emmeans(a1, c("stimulus"), by="task"))

update(pairs(ls1), by=NULL, adjust = "holm")

pairs(update(pairs(ls1), by=NULL))

test(pairs(update(pairs(ls1), by=NULL)), joint=TRUE)

lsmip(a1, task ~ stimulus)

lsm1 <- summary(lsmeans(a1, c("stimulus","task")))
lsm1$lsmean <- exp(lsm1$lsmean)
require(lattice)
xyplot(lsmean ~ stimulus, lsm1, group = task, type = "b", 
       auto.key = list(space = "right"))