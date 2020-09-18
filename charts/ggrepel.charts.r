library(wesanderson)
library(colorspace)
library(ggpubr)
library(ggrepel)
library(patchwork)

head(iris)

gghistogram(iris, x="Sepal.Length", color="Species", fill="Species", palette = wes_palette("Darjeeling1"))

ggdensity(iris, x="Sepal.Length", color="Species", fill="Species", palette = wes_palette("Darjeeling1"), add="mean")

ggboxplot(iris, x = "Species", y = "Sepal.Length", color = "Species", palette = qualitative_hcl(3, palette = "Dynamic"), add = "jitter", shape = "Species")

head(mtcars)
mtcars$mpg_dev <- (mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg) 
mtcars$name <- row.names(mtcars) 
mtcars$cyl_discr <- as.factor(mtcars$cyl)

ggdotchart(mtcars, 
           x = "name", 
           y = "mpg_dev", 
           color = "cyl_discr", 
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
           sorting = "descending", 
           add = "segments", 
           add.params = list(color = "lightgray", size = 1), 
           group = "cyl_discr", 
           dot.size = 4 ) + 
  geom_hline(yintercept = 0, linetype = 2, color = "lightgray")

ggdotchart(mtcars, 
           x = "name", 
           y = "mpg_dev", 
           color = "cyl_discr", 
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
           sorting = "descending", 
           add = "segments", 
           add.params = list(color = "lightgray", size = 1), 
           group = "cyl_discr", 
           dot.size = 4,
           rotate = TRUE) + 
  geom_hline(yintercept = 0, linetype = 2, color = "lightgray")

ggdotchart(mtcars, x = "name", y = "mpg_dev",
           color = "cyl_discr",                                
           palette = c("#00AFBB", "#E7B800", "#FC4E07"),
           sorting = "descending",                       
           add = "segments",                             
           add.params = list(color = "lightgray", size = 1), 
           group = "cyl_discr",                                
           dot.size = 2,
           rotate = TRUE,
           title = "Miles per Galon",
           subtitle = "Per number of cylinders",
           caption = "Package: https://cran.r-project.org/web/packages/ggpubr/index.html"
) +
  geom_hline(yintercept = 0, linetype = 2, color = "lightgray") + 
  font("y.text", size = 5) + 
  font("title", color = darken("#00AFBB", amount = 0.3)) + 
  font("caption", face = "italic")

ggscatter(mtcars, x = "wt", y = "mpg", color = "cyl_discr", palette = "jco", shape = "cyl_discr")

ggscatter(mtcars, x = "wt", y = "mpg", color = "cyl_discr", palette = "jco", shape = "cyl_discr", label = "name", repel = TRUE)

p1 <- ggscatter(mtcars, x = "wt", y = "mpg",
                color = "cyl_discr", palette = "jco",           
                shape = "cyl_discr", label = "name", repel = TRUE,
                label.select = dplyr::filter(mtcars, mpg > 25 | wt > 5) %>% .$name)
library(plotly)
ggplotly(p1)

p2 <- ggscatter(mtcars, x = "wt", y = "mpg",
                color = "cyl_discr", palette = "jco",           
                shape = "cyl_discr", add = "reg.line", conf.int=TRUE)
p2

