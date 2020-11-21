# install.packages("remotes")
remotes::install_github("jthomasmock/espnscrapeR")
library(espnscrapeR)
library(tidyverse)
library(gtable)
library(gt)
devtools::install_github("rstudio/fontawesome")
library(fontawesome)
library(kableExtra)
library(purrr)

nfl_qbr <- espnscrapeR::get_nfl_qbr(2020) %>% 
  slice(1:10)

ex_tab <- nfl_qbr %>% 
  select(rank, last_name, team, qbr_total, qb_plays, pass, run) %>% 
  gt() %>% 
  tab_header(
    title = gt::html("<span style='color:red'>ESPN's QBR for 2020</span>")
  ) %>% 
  cols_label(
    qbr_total = gt::html(
      "<span style ='font-weight:bold;font-size:20px'>QBR</span>")
  )
ex_tab

ex_tab <- nfl_qbr %>% 
  select(rank, last_name, team, qbr_total, qb_plays, pass, run) %>% 
  gt() 

ex_tab %>%
  cols_merge(
    columns = vars(last_name, team)
  )

ex_tab %>%
  cols_merge(
    columns = vars(last_name, team)
  ) %>% 
  text_transform(
    locations = cells_body(
      columns = vars(last_name)
    ),
    fn = function(x){
      name <- word(x, 1)
      team <- word(x, -1)
      glue::glue(
        "<div><span style='font-weight:bold;font-variant:small-caps;font-size:14px'>{name}</div>
        <div><span style ='font-weight:bold;color:grey;font-size:10px'>{team}</span></div>"
      )
    }
  )

ex_tab %>%
  cols_merge(
    columns = vars(last_name, team)
  ) %>% 
  text_transform(
    locations = cells_body(
      columns = vars(last_name)
    ),
    fn = function(x){
      name <- word(x, 1)
      team <- word(x, -1)
      glue::glue(
        "<div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;font-size:14px'>{name}</div>
        <div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>{team}</span></div>"
      )
    }
  ) %>% 
  tab_options(
    data_row.padding = px(5),
  )

# combined

# function to incorporate player name + team
combine_word <- function(name, team){
  glue::glue(
    "<div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;font-size:14px'>{name}</div>
        <div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>{team}</span></div>"
  )
}

nfl_qbr %>% 
  select(rank, short_name, team, qbr_total, qb_plays, pass, run) %>% 
  mutate(
    combo = combine_word(short_name, team),
    combo = map(combo, gt::html)
  ) %>% 
  select(rank, combo, everything(), -short_name, -team) %>% 
  gt() %>% 
  cols_align(
    align = "left",
    columns = vars(combo)
  ) %>% 
  tab_options(
    data_row.padding = px(5)
  )


# first split the data by cylinders
mpg_list <- split(mtcars$mpg, mtcars$cyl)

mpg_list

# cool boxplot

# pipe the 
data.frame(
  cyl = c(4,6,8),
  boxplot = "") %>% 
  kbl(booktabs = TRUE) %>%
  kable_paper(full_width = FALSE) %>% column_spec(2, image = spec_boxplot(mpg_list, width = 300, height = 70))

mtcars %>% 
  group_by(cyl) %>% 
  summarize(data = list(mpg), .groups = "drop")

mpg_rng <- range(mtcars$mpg)

mtcars %>% 
  group_by(cyl) %>% 
  summarize(data = list(mpg), .groups = "drop") %>% 
  mutate(
    plot = map(data, ~spec_plot(.x, ylim = mpg_rng, same_lim = TRUE, width = 300, height = 70)),
    plot = map(plot, "svg_text"),
    plot = map(plot, gt::html)
  ) %>% 
  select(-data) %>% 
  gt()

mpg_list %>% str()

spec_plot(mpg_list) %>% 
  pluck("4") %>% 
  str()

mtcars %>% 
  group_by(cyl) %>% 
  summarize(data = list(mpg), .groups = "drop") %>% 
  mutate(
    plot = map(data, ~spec_plot(.x, ylim = mpg_rng, same_lim = TRUE, width = 300, height = 70)),
    plot = map(plot, "svg_text"),
    plot = map(plot, gt::html)
  ) %>% 
  select(-data) %>% 
  gt()

# with gtplot

gt_plot <- function(table_data, column, plot_data, plot_fun, ...){
  text_transform(
    table_data,
    # note the use of {{}} here - this is tidy eval
    # that allows you to indicate specific columns
    locations = cells_body(columns = vars({{column}})),
    fn = function(x){
      plot <- map(plot_data, plot_fun, width = 300, height = 70, same_lim = TRUE, ...)
      plot_svg <- map(plot, "svg_text")
      map(plot_svg, gt::html)
    }
  )
}

tibble(cyl = c(4,6,8), boxplot = "") %>% 
  gt() %>% 
  gt_plot(
    column = boxplot,  # column to create plot in 
    plot_data = mpg_list, # external data to reference
    plot_fun = spec_boxplot,  # which plot fun
    lim = mpg_rng # range applied
  )

tibble(cyl = c(4,6,8), boxplot = "") %>% 
  gt() %>% 
  gt_plot(
    column = boxplot,  # column to create plot in 
    plot_data = mpg_list, # external data to reference
    plot_fun = spec_plot,  # which plot fun
    ylim = mpg_rng, # range applied,
    col = "black", # change color of line
    cex = 5 # change size of points
  )

tibble(
  cyl = c(4,6,8), 
  boxplot = "", mpg_hist = "", mpg_line1 = "", 
  mpg_line2 = "", mpg_points1 = "", 
  mpg_points2 = "", mpg_poly = ""
) %>% 
  gt() %>% 
  gt_plot(column = boxplot, plot_data = mpg_list, plot_fun = spec_boxplot, lim = mpg_rng) %>% 
  gt_plot(column = mpg_hist, plot_data = mpg_list, plot_fun = spec_hist, lim = mpg_rng) %>% 
  gt_plot(column = mpg_line1, plot_data = mpg_list, plot_fun = spec_plot, ylim = mpg_rng) %>% 
  gt_plot(column = mpg_line2, plot_data = mpg_list, plot_fun = spec_plot) %>% 
  gt_plot(column = mpg_points1, plot_data = mpg_list, plot_fun = spec_plot, type = "p", ylim = mpg_rng, cex = 4) %>% 
  gt_plot(column = mpg_points2, plot_data = mpg_list, plot_fun = spec_plot, type = "p", cex = 4) %>% 
  gt_plot(column = mpg_poly, plot_data = mpg_list, plot_fun = spec_plot, polymin = 5, ylim = mpg_rng)


# works now
mtcars %>% 
  group_by(cyl) %>% 
  summarize(mpg_data = list(mpg), .groups = "drop") %>% 
  gt() %>% 
  text_transform(
    locations = cells_body(columns = vars(mpg_data)),
    fn = function(x){
      data_in = pluck(., "_data", "mpg_data")
      plot = map(data_in, ~spec_plot(.x, ylim = mpg_rng, same_lim = TRUE, width = 300, height = 70))
      plot = map_chr(plot, "svg_text")
    })

# WORKS
mtcars %>%
  group_by(cyl) %>%
  summarize(mpg_data = list(as.double(mpg)), .groups = "drop") %>%
  gt() %>%
  text_transform(
    locations = cells_body(columns = vars(mpg_data)),
    fn = function(x) {
      # split the strings at each comma
      split_data <- str_split(x, ", ")
      # convert to type double
      data <- map(split_data, as.double)
      # create the plot
      plot <- map(data, ~ spec_plot(.x, ylim = mpg_rng, same_lim = TRUE, width = 300, height = 70))
      # extract the svg item
      map(plot, "svg_text")
    }
  )

gt_plot <- function(table_data, plot_col, data_col, plot_fun, ...){
  # save the data extract ahead of time 
  # to be used in our anonymous function below
  data_in = pluck(table_data, "_data", data_col)
  
  text_transform(
    table_data,
    # note the use of {{}} here - this is tidy eval
    # that allows you to indicate specific columns
    locations = cells_body(columns = vars({{plot_col}})),
    fn = function(x){
      plot <- map(data_in, plot_fun, width = 300, height = 70, same_lim = FALSE, ...)
      plot_svg <- map(plot, "svg_text")
      map(plot_svg, gt::html)
    }
  )
}

# works!
mtcars %>% 
  group_by(cyl) %>% 
  summarize(mpg_data = list(mpg), .groups = "drop") %>% 
  gt() %>% 
  # note you can leave mpg_data unquoted for the tidyeval
  # but have to quote mpg_data for the pluck
  gt_plot(mpg_data, "mpg_data", plot_fun = spec_plot)


# interactive/need to find function spk_chr
library(sparkline)

sparkline(0)

gt_spark <- function(table_data, plot_col, data_col){
  # save the data extract ahead of time 
  # to be used in our anonymous function below
  data_in = pluck(table_data, "_data", data_col)
  
  text_transform(
    table_data,
    # note the use of {{}} here - this is tidy eval
    # that allows you to indicate specific columns
    locations = cells_body(columns = vars({{plot_col}})),
    fn = function(x){
      sparkline_plot <- map(
        data_in, 
        ~spk_chr(values = .x, chartRangeMin = 0)
      )
      
      map(sparkline_plot, gt::html)
    }
  )
}

tibble(
  var = c("mpg", "wt"),
  sparkline1 = "",
  sparkline2 = "",
  box = ""
) %>% 
  gt() %>% 
  text_transform(
    locations = cells_body(vars(sparkline1)),
    fn = function(x){
      sparkline <- map(list(mtcars$mpg, mtcars$wt), ~spk_chr(values = .x, chartRangeMin = 0))
      map(sparkline, gt::html)
    }
  ) %>% 
  text_transform(
    locations = cells_body(vars(sparkline2)),
    fn = function(x){
      sparkline <- map(list(mtcars$mpg, mtcars$wt), ~spk_chr(values = .x, type = "bar", chartRangeMin = 0))
      map(sparkline, gt::html)
    }
  ) %>% 
  text_transform(
    locations = cells_body(vars(box)),
    fn = function(x){
      sparkline <- map(list(mtcars$mpg, mtcars$wt), ~spk_chr(values = .x, type = "box", chartRangeMin = 0))
      map(sparkline, gt::html)
    }
  )

# works!
mtcars %>% 
  group_by(cyl) %>% 
  summarize(mpg_data = list(mpg), .groups = "drop") %>% 
  gt() %>% 
  # note you can leave mpg_data unquoted for the tidyeval
  # but have to quote mpg_data for the pluck
  gt_spark(mpg_data, "mpg_data")

# forest
coef_table <- data.frame(
  Variables = c("var 1", "var 2", "var 3"),
  Coefficients = c(1.6, 0.2, -2.0),
  Conf.Lower = c(1.3, -0.4, -2.5),
  Conf.Higher = c(1.9, 0.6, -1.4)
) 

tibble(
  Variable = coef_table$Variables,
) %>%
  mutate(
    image = spec_pointrange(
      x = coef_table$Coefficients, 
      xmin = coef_table$Conf.Lower, 
      xmax = coef_table$Conf.Higher, 
      vline = 0,
      width = 250,
      cex = .75,
      col = "black",
      pch = 16
    )
  ) %>% 
  mutate(
    image = map(image, "svg_text"),
    image = map(image, ~gt::html(as.character(.x)))
  ) %>% 
  gt()


# with fake data
coef_table <- tibble(
  group = c(
    "",
    rep("Sex", 2),
    rep("Age", 4),
    rep("Body-Mass index", 2),
    rep("Race", 3),
    rep("Baseline statin treatment", 2),
    rep("Intensity of statin treatment", 2),
    rep("Metabolic disease", 3),
    rep("Renal function", 3)
  ),
  subgroup = c(
    "All Patients",
    "Male", "Female",
    "<65 yr", ">= 65 yr", "<75 yr", ">=75 yr",
    "<=Median", ">Median",
    "White", "Black", "Other",
    "Yes", "No",
    "High", "Not high",
    "Diabetes", "Metabolic syndrome", "Neither",
    "Normal", "Mild impairment", "Moderate impairment"
  ),
  Inclisiran = c(
    781, 535,246,297,484,638,143,394,387,653,110,18,701,80,538,243,371,195,215,395,269,113
  ),
  Placebo = c(
    780,548,232,333,447,649,131,385,394,685,87,8,692,88,546,234,331,207,242,410,260,107
  ),
  coefficients = c(-60,-55,-68,-58,-55,-57,-58,-55,-48,-58,-57,-49,-44,-58,-55,-57,-54,-52,-54,-53, -54,-52)
) %>% 
  mutate(
    conf_range = runif(22, min = 5, max = 10),
    conf_lower = coefficients - conf_range,
    conf_higher = coefficients + conf_range
  ) %>%
  mutate(
    image = spec_pointrange(
      x = coefficients, 
      xmin = conf_lower, 
      xmax = conf_higher, 
      same_lim = TRUE,
      lim = c(-100, 25),
      vline = 0,
      width = 550,
      cex = .75,
      col = "black"
    )
  )



coef_table %>% 
  select(-coefficients, -contains("conf")) %>% 
  mutate(
    image = map(image, "svg_text"),
    image = map(image, ~gt::html(as.character(.x)))
  ) %>% 
  select(group:Placebo, pct_diff = image) %>% 
  gt(
    groupname_col = "group",
    rowname_col = "subgroup"
  ) %>% 
  opt_row_striping() %>% 
  tab_options(
    data_row.padding = px(3)
  )

# tool tip
library(htmltools)

# Add tooltip to column labels
with_tooltip <- function(value, tooltip) {
  tags$abbr(style = "text-decoration: underline; text-decoration-style: solid; cursor: question; color: blue",
            title = tooltip, value)
}


# note you could use ANY font-awesome logo
# https://fontawesome.com/cheatsheet
rating_stars <- function(rating, max_rating = 5) {
  rounded_rating <- floor(rating + 0.5)  # always round up
  stars <- lapply(seq_len(max_rating), function(i) {
    if (i <= rounded_rating) fontawesome::fa("star", fill= "orange") else fontawesome::fa("star", fill= "grey")
  })
  label <- sprintf("%s out of %s", rating, max_rating)
  div_out <- div(title = label, "aria-label" = label, role = "img", stars)
  
  as.character(div_out) %>% 
    gt::html()
}

rank_chg <- function(change_dir){
  if (change_dir == "increase") {
    logo_out <- fontawesome::fa("arrow-up", fill = "blue")
  } else if (change_dir == "decrease"){
    logo_out <- fontawesome::fa("arrow-down", fill = "red")
  }
  
  logo_out %>% 
    as.character() %>% 
    gt::html()
  
}

bar_chart <- function(value, color = "red"){
  
  glue::glue("<span style=\"display: inline-block; direction: ltr; border-radius: 4px; padding-right: 2px; background-color: {color}; color: {color}; width: {value}%\"> &nbsp; </span>") %>% 
    as.character() %>% 
    gt::html()
}


add_cyl_color <- function(cyl){
  add_color <- if (cyl == 4) {
    "background: hsl(116, 60%, 90%); color: hsl(116, 30%, 25%);"
  } else if (cyl == 6) {
    "background: hsl(230, 70%, 90%); color: hsl(230, 45%, 30%);"
  } else if (cyl == 8) {
    "background: hsl(350, 70%, 90%); color: hsl(350, 45%, 30%);"
  }
  div_out <- htmltools::div(
    style = paste(
      "display: inline-block; padding: 2px 12px; border-radius: 15px; font-weight: 600; font-size: 12px;",
      add_color
    ),
    paste(cyl, "Cylinders")
  )
  
  as.character(div_out) %>% 
    gt::html()
}

set.seed(377)

mtcars %>% 
  tibble() %>% 
  select(1:4) %>% 
  sample_n(size = 6) %>% 
  mutate(
    rank_change = sample(c("increase", "decrease"), size = 6, replace = TRUE),
    rank_change = map(rank_change, rank_chg)
  ) %>% 
  mutate(
    rating = sample(1:5, size = 6, replace = TRUE),
    rating = map(rating, rating_stars)
  ) %>% 
  mutate(
    cylinder = map(cyl, add_cyl_color)
  ) %>% 
  mutate(
    mpg_plot = mpg/max(mpg) * 100,
    mpg_plot = map(mpg_plot, ~bar_chart(value = .x, color = "lightblue"))
  ) %>% 
  gt() %>% 
  cols_align(
    align = "left",
    columns = vars(mpg_plot)
  ) %>% 
  cols_label(
    mpg = gt::html(as.character(with_tooltip("MPG", "Miles per Gallon")))
  ) %>% 
  tab_source_note(
    source_note = html(
      htmltools::tags$a(
        href = "https://gt.rstudio.com/reference/md.html", 
        target = "_blank", 
        "Data Source"
      ) %>% 
        as.character()
    )
  ) %>% 
  tab_source_note(
    source_note = html(
      "<details><h3 style='font-face:bold'>Table Key</h3><div>MPG: Miles Per Gallon</div><div>Cyl: Cylinders</div><div>disp: Displacement</div><div>hp: Horsepower</div><div>rank_change: Rank Change</div><div>rating: Rating</div></details>"
    )
  ) %>% 
  tab_options(
    data_row.padding = px(5)
  )

