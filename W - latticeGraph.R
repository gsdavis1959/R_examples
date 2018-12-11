library(lattice)
df <- read.csv("~/Data/Datasets/lattice_data.csv",
               header = T, sep = ";")
mypch <- rep(21, 3)
mycol <- c(rgb(0.2, 0.8, 0.9), rgb(0.8, 0.2, 0.9), rgb(0.9, 0.2, 0.2))

stripplot(mean ~ group1 | item_parc, groups = group2, data = df,
          type = c("a", "p"), strip = strip.custom(strip.names = c(F, T)),
          ylab = "Score-Mittelwert", layout = c(2, 4),
          scales = list(y = list(cex = 0.9),
                        x = list(cex = 1.1, labels = c("männl.", "weibl."),
                                 tck = c(1, 0))),
          par.strip.text = list(cex = 0.9),
          panel = function(...) {
            panel.stripplot(..., col = mycol, pch = mypch, fill = mycol)
          },
          index.cond = list(c(7,8,5,6,3,4,1,2)),
          key = list(space = "bottom", text = list(c("Alter < 35 J.",
                                                     "Alter 35-50 J.",
                                                     "Alter > 50 J.")),
                     points = list(col = mycol, cex = 0.8, pch = mypch, fill = mycol),
                     rep = F))
