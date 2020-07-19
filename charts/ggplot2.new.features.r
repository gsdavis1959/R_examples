install.packages("ggplot2")
library(ggplot2)
p <- ggplot(mpg) +
  geom_bar(aes(x = manufacturer)) + 
  theme(axis.text.x = element_text(size = 11))

# x axis formatting
p + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

p <- ggplot(mpg) + 
  geom_point(aes(displ, cty, size = hwy, colour = hwy))

# bins
p + 
  scale_size_binned()

p + 
  scale_colour_binned()

p + 
  scale_size_binned(guide = guide_bins(show.limits = TRUE))

p + 
  scale_x_binned()

# histograms
ggplot(mpg) + 
  geom_bar(aes(displ)) + 
  scale_x_binned()

ggplot(mpg) + 
  geom_bar(aes(x = manufacturer)) + 
  coord_flip()

ggplot(mpg) + 
  geom_bar(aes(y = manufacturer))

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth(orientation = "y")

ggplot(mpg, aes(class, hwy)) +
  geom_boxplot(aes(colour = class, fill = after_scale(alpha(colour, 0.4))))

ggplot(mpg) + 
  geom_bar(
    aes(
      x = drv, 
      colour = stage(start = drv, after_scale = alpha(colour, 0.5))
    ), 
    fill = NA, size = 4
  )

# themes
register_theme_elements(
  ggrelfacet.panel.arrow = element_line(
    size = 3, arrow = arrow()
  ),
  element_tree = list(
    ggrelfacet.panel.arrow = el_def("element_line", "line")
  )
)

volcano_long <- data.frame(
  x = as.vector(col(volcano)),
  y = as.vector(row(volcano)),
  z = as.vector(volcano)
)

ggplot(volcano_long, aes(x, y, z = z)) + 
  geom_polygon(aes(fill = stat(level)), alpha = 0.5, stat = "contour") + 
  guides(fill = "legend")

ggplot(volcano_long, aes(x, y, z = z)) + 
  geom_contour_filled(aes(fill = stat(level)), alpha = 0.5)

ggplot(volcano_long, aes(x, y, z = z)) + 
  geom_contour_filled(aes(fill = stat(level))) + 
  guides(fill = guide_colorsteps(barheight = unit(10, "cm")))

# titles
ggplot(mpg) + 
  geom_point(aes(hwy, displ)) + 
  ggtitle("The placement of this title may surprise you") + 
  theme(plot.title.position = "plot")

# lines
huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
ggplot(huron, aes(year)) + 
  geom_ribbon(aes(ymin = level - 10, ymax = level + 10), fill = "grey", colour = "black")

# density
ggplot(diamonds, aes(carat)) +
  geom_density(fill = "blue")
