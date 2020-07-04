library(ggplot2)
library(tidyverse, warn.conflicts = FALSE)
library(ggupset)
library(plotly)

head(tidy_movies)

tidy_movies %>%
  distinct(title, year, length, .keep_all=TRUE) %>%
  ggplot(aes(x=Genres)) +
  geom_bar() +
  scale_x_upset(n_intersections = 20)


data("gene_pathway_membership")
gene_pathway_membership[, 1:7]

# convert to a matrix

tidy_pathway_member <- gene_pathway_membership %>%
  as_tibble(rownames = "Pathway") %>%
  gather(Gene, Member, -Pathway) %>%
  filter(Member) %>%
  select(- Member)

tidy_pathway_member

# create multiple pathways for each gene in a list

tidy_pathway_member %>%
  group_by(Gene) %>%
  summarize(Pathways = list(Pathway))


# plot
tidy_pathway_member %>%
  group_by(Gene) %>%
  summarize(Pathways = list(Pathway)) %>%
  ggplot(aes(x = Pathways)) +
  geom_bar() +
  scale_x_upset()

tidy_movies %>%
  distinct(title, year, length, .keep_all=TRUE) %>%
  mutate(Genres_collapsed = sapply(Genres, function(x) paste0(sort(x), collapse = "-"))) %>%
  select(title, Genres, Genres_collapsed)

tidy_movies %>%
  distinct(title, year, length, .keep_all=TRUE) %>%
  mutate(Genres_collapsed = sapply(Genres, function(x) paste0(sort(x), collapse = "-"))) %>%
  ggplot(aes(x=Genres_collapsed)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))

tidy_movies %>%
  distinct(title, year, length, .keep_all=TRUE) %>%
  ggplot(aes(x=Genres)) +
  geom_bar() +
  scale_x_mergelist(sep = "-") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))

tidy_movies %>%
  distinct(title, year, length, .keep_all=TRUE) %>%
  ggplot(aes(x=Genres)) +
  geom_bar() +
  scale_x_mergelist(sep = "-") +
  axis_combmatrix(sep = "-")

# order by degree
tidy_movies %>%
  distinct(title, year, length, .keep_all=TRUE) %>%
  ggplot(aes(x=Genres)) +
  geom_bar() +
  scale_x_upset(order_by = "degree")

tidy_movies %>%
  distinct(title, year, length, .keep_all=TRUE) %>%
  ggplot(aes(x=Genres)) +
  geom_bar(fill = 'darkblue') +
  scale_x_upset(order_by = "degree") +
  theme_combmatrix(combmatrix.panel.point.color.fill = "skyblue",
                   combmatrix.panel.line.size = 0,
                   combmatrix.label.make_space = FALSE)

# UpSetR
tidy_movies %>%
  distinct(title, year, length, .keep_all=TRUE) %>%
  unnest(cols = Genres) %>%
  mutate(GenreMember=1) %>%
  pivot_wider(names_from = Genres, values_from = GenreMember, values_fill = list(GenreMember = 0)) %>%
  as.data.frame() %>%
  UpSetR::upset(sets = c("Action", "Romance", "Short", "Comedy", "Drama"), keep.order = TRUE)

# ggupset
tidy_movies %>%
  distinct(title, year, length, .keep_all=TRUE) %>%
  ggplot(aes(x=Genres)) +
  geom_bar() +
  scale_x_upset(order_by = "degree", n_sets = 5)

