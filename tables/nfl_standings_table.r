# install.packages("remotes")
library(gt)
library(espnscrapeR)
library(tidyverse)
library(gtable)

# devtools::install_github("rstudio/fontawesome")
library(fontawesome)
library(kableExtra)
library(purrr)
library(glue)
library(sparkline)


# use espnscrapeR to get NFL standings + QBR ratings
nfl_qbr <- get_nfl_qbr(2020)
nfl_standings <- get_nfl_standings(2020)

# also get weekly for embedded plot
qbr_weekly <- crossing(season = 2020, week = 1:10) %>%
  pmap_dfr(.f = get_nfl_qbr)

qbr_match <- qbr_weekly %>%
  filter(short_name %in% nfl_qbr$short_name) %>%
  group_by(short_name, team) %>%
  summarise(qbr_weekly = list(qbr_total), .groups = "drop",
            qbr = mean(qbr_total),
            qbr_sd = sd(qbr_total),
            plays = sum(qb_plays),
            pass = mean(pass),
            run = mean(run),
            head = unique(headshot_href),
            n = n()) %>%
  arrange(desc(qbr)) %>% 
  filter(n >= 7)

# clean up the data a bit and combine
tab_df <- qbr_match %>% 
  left_join(nfl_standings, by = c("team" = "abb_name")) %>%
  select(short_name, team, head, qbr_weekly:run, wins, losses, points_for) %>%
  mutate(wl = glue("{wins}-{losses}")) %>%
  select(-wins, -losses)
tab_df

qbr_rnk_chg <- qbr_weekly %>% 
  mutate(game_week = as.integer(game_week)) %>% 
  group_by(short_name) %>% 
  mutate(mean_qbr = mean(qbr_total)) %>% 
  ungroup() %>% 
  select(game_week, rank, short_name, qbr_total, mean_qbr) %>% 
  filter(game_week != max(game_week)) %>% 
  filter(short_name %in% nfl_qbr$short_name) %>%
  group_by(short_name) %>%
  summarize(prev_qbr = mean(qbr_total), mean_qbr = unique(mean_qbr)) %>% 
  mutate(
    prev_week = rank(-prev_qbr),
    rank = rank(-mean_qbr)
  ) %>% 
  mutate(rank_chg = prev_week-rank) %>% 
  ungroup() %>% 
  arrange(desc(mean_qbr)) %>% 
  select(short_name, qbr = mean_qbr, rank_chg, rank)

qbr_rnk_chg


combine_word <- function(name, team, wl){
  glue::glue(
    "<div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;font-size:14px'>{name}</div>
        <div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>{team}&nbsp;&nbsp;{wl}</span></div>"
  )
}

combo_df <- tab_df %>% 
  left_join(qbr_rnk_chg, by = c("short_name", "qbr")) %>%
  select(rank, rank_chg, short_name:wl) %>% 
  mutate(
    rank = row_number(),
    combo = combine_word(short_name, team, wl),
    combo = map(combo, gt::html)
  ) %>% 
  select(rank, rank_chg, head, combo, qbr, qbr_weekly, plays, points_for)

combo_df

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


final_table <- combo_df %>% 
  gt() %>% 
  cols_align(
    align = "left",
    columns = vars(combo)
  ) %>% 
  tab_options(
    data_row.padding = px(2)
  ) %>% 
  text_transform(
    locations = cells_body(columns = vars(head)),
    fn = function(x){
      gt::web_image(x)
    }
  ) %>% 
  text_transform(
    locations = cells_body(columns = vars(rank_chg)),
    fn = function(x){
      
      rank_chg <- as.integer(x)
      
      choose_logo <-function(x){
        if (x == 0){
          gt::html(fontawesome::fa("equals", fill = "grey"))
        } else if (x > 0){
          gt::html(glue::glue("<span style='color:#1134A6;font-face:bold;font-size:10px;'>{x}</span>"), fontawesome::fa("arrow-up", fill = "#1134A6"))
        } else if (x < 0) {
          gt::html(glue::glue("<span style='color:#DA2A2A;font-face:bold;font-size:10px;'>{x}</span>"), fontawesome::fa("arrow-down", fill = "#DA2A2A"))
        }
      } 
      
      map(rank_chg, choose_logo)
      
    }
  ) %>% 
  fmt_number(
    columns = vars(qbr), 
    decimals = 1
  ) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(TRUE)
  ) %>% 
  cols_label(
    rank = "RK",
    combo = "",
    head = "QB",
    qbr = "QBR",
    plays = "PLAYS",
    points_for = "PF",
    qbr_weekly = "WEEKLY",
    rank_chg = ""
  ) %>% 
  gt_spark(qbr_weekly, "qbr_weekly") %>%
  espnscrapeR::gt_theme_espn() %>% 
  tab_source_note(
    source_note = gt::html(
      htmltools::tags$a(
        href = "https://www.espn.com/nfl/qbr", 
        target = "_blank", 
        "Data: ESPN"
      ) %>% 
        as.character()
    )
  ) %>% 
  cols_align(
    "left",
    columns = vars(qbr_weekly)
  ) %>% 
  cols_width(
    vars(rank) ~ px(25),
    vars(rank_chg) ~ px(35),
    vars(head) ~ px(50),
    vars(combo) ~ px(115),
    vars(qbr) ~ px(35),
    vars(plays) ~ px(35),
    vars(points_for) ~ px(35),
    vars(qbr_weekly) ~ px(75)
  ) %>% 
  tab_header(
    title = gt::html("<h3>NFL QBR through Week 10</h3>")
  ) %>% 
  tab_options(
    table.width = px(480),
    data_row.padding = px(4)
  )
final_table
