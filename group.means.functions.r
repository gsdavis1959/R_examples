libs <- c('dplyr', 'stringr',             # wrangling
          'knitr','kableExtra',           # table styling
          'ggplot2','ggforce')            # plots
invisible(lapply(libs, library, character.only = TRUE))
data("diamonds")

group_mean <- function(df, g, x, y){
  
  group_cols <- enquos(x, y)
  mean_col <- enquo(g)
  df %>% 
    group_by(!!! group_cols) %>% 
    summarise(mean = mean(!! mean_col))
}

group_mean(diamonds, price, cut, color) %>% 
  head(5)

# with dots
group_mean <- function(df, g, ...){
  
  mean_col <- enquo(g)
  df %>% 
    group_by(...) %>% 
    summarise(mean = mean(!! mean_col))
}

group_mean(diamonds, price, cut, color) %>% 
  head(5)

# with a custom name
group_mean <- function(df, g, n, ...){
  
  mean_col <- enquo(g)
  new_name <- enquo(n)
  
  df %>% 
    group_by(...) %>% 
    summarise(!! new_name := mean(!! mean_col))
}

group_mean(diamonds, price, mean_price, cut, color) %>% 
  head(5)

# scatter plot
plot_xy <- function(df, x, y, col, var_zoom, ...){
  
  x <- enquo(x)
  y <- enquo(y)
  col <- enquo(col)
  group_vars <- enquos(...)
  
  dfname <- ensym(df) %>% str_to_sentence()
  xname <- ensym(x) %>% str_to_sentence()
  yname <- ensym(y) %>% str_to_sentence()
  colname <- ensym(col) %>% str_to_sentence()
  
  df %>% 
    mutate(!! col := as.factor(!! col)) %>% 
    group_by(!! col, !!! group_vars) %>% 
    summarise(mean_x = mean(!!x),
              mean_y = mean(!!y)) %>% 
    ungroup() %>% 
    ggplot(aes(mean_x, mean_y, col = !!col)) +
    geom_point() +
    scale_color_brewer(type = "qual", palette = "Set1") +
    labs(x = xname, y = yname, col = colname) +
    ggtitle(str_c(dfname, " dataset: ",
                  xname, " vs ", yname,
                  " with colour coding by ", colname),
            subtitle = str_c("Zoom view to emphasise ",
                             colname, " = ", var_zoom)) +
    facet_zoom(x = (!! col == var_zoom))
}

plot_xy(diamonds, carat, price, clarity, "IF", color, cut)

