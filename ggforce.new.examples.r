library(ggforce)

ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))
values <- data.frame(
  id = ids,
  value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
)
positions <- data.frame(
  id = rep(ids, each = 4),
  x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
        0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
  y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
        2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
)
datapoly <- merge(values, positions, by = c("id"))

# Standard look
ggplot(datapoly, aes(x = x, y = y)) +
  geom_polygon(aes(fill = value, group = id))

library(plotly)
# Contracted and rounded

ggplot(datapoly, aes(x = x, y = y)) +
  geom_shape(aes(fill = value, group = id), 
             expand = unit(-2, 'mm'), radius = unit(5, 'mm'))

ggplot() +
  geom_regon(aes(x0 = runif(8), y0 = runif(8), sides = sample(3:10, 8),
                 angle = 0, r = runif(8) / 10)) +
  coord_fixed()

data <- data.frame(
  x = c(1, 2, 2, 1, 2, 3, 3, 2),
  y = c(1, 2, 3, 2, 3, 1, 2, 5),
  group = c(1, 1, 1, 1, 2, 2, 2, 2)
)

ggplot(data) +
  geom_diagonal_wide(aes(x, y, group = group))

# sanky diagram

titanic <- reshape2::melt(Titanic)
# This is how we usually envision data for parallel sets
head(titanic)

# Reshape for putting the first 4 columns as axes in the plot
titanic <- gather_set_data(titanic, 1:4)
head(titanic)

# Do the plotting
ggplot(titanic, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Sex), alpha = 0.3, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'white')


# annotation
ggplot(iris, aes(Petal.Length, Petal.Width)) +
  geom_mark_ellipse(aes(fill = Species, label = Species)) +
  geom_point()


desc <- 'This iris species has a markedly smaller petal than the others.'
ggplot(iris, aes(Petal.Length, Petal.Width)) +
  geom_mark_ellipse(aes(filter = Species == 'setosa', label = 'Setosa', 
                        description = desc)) +
  geom_point()

# network

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_delaunay_tile(alpha = 0.3) + 
  geom_delaunay_segment2(aes(colour = Species, group = -1), size = 2,
                         lineend = 'round')

ggplot(iris, aes(Sepal.Length, Sepal.Width)) + 
  geom_voronoi_tile(aes(fill = Species, group = -1L)) + 
  geom_voronoi_segment() +
  geom_point()

ggplot(iris, aes(Sepal.Length, Sepal.Width)) + 
  geom_voronoi_tile(aes(fill = Species), colour = 'black')

ggplot(iris, aes(Sepal.Length, Sepal.Width)) + 
  geom_voronoi_tile(aes(fill = Species, group = -1L), max.radius = 0.2,
                    colour = 'black')

ggplot(iris, aes(Sepal.Length, Sepal.Width)) + 
  geom_voronoi_tile(aes(fill = Species, group = -1L), max.radius = 1,
                    colour = 'black', expand = unit(-0.5, 'mm'), 
                    radius = unit(0.5, 'mm'), show.legend = FALSE)
# zoom
ggplot(diamonds) + 
  geom_histogram(aes(x = price), bins = 50) + 
  facet_zoom(xlim = c(50, 2000), ylim = c(0, 10000), horizontal = FALSE)

ggplot() + 
  geom_histogram(aes(x = price), dplyr::mutate(diamonds, z = FALSE), bins = 50) + 
  geom_histogram(aes(x = price), dplyr::mutate(diamonds, z = TRUE), bins = 500) + 
  facet_zoom(xlim = c(3000, 5000), ylim = c(0, 300), zoom.data = z,
             horizontal = FALSE) + 
  theme(zoom.y = element_blank(), validate = FALSE)
