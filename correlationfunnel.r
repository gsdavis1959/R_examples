library(correlationfunnel)
library(dplyr)
# Use ?marketing_campagin_tbl to get a description of the marketing campaign features
data("marketing_campaign_tbl")

marketing_campaign_tbl %>% glimpse()

marketing_campaign_binarized_tbl <- marketing_campaign_tbl %>%
  select(-ID) %>%
  binarize(n_bins = 4, thresh_infreq = 0.01)

marketing_campaign_binarized_tbl %>% 
  slice(1:10) %>%
  knitr::kable()

marketing_campaign_correlated_tbl <- marketing_campaign_binarized_tbl %>%
  correlate(target = TERM_DEPOSIT__yes)

marketing_campaign_correlated_tbl

marketing_campaign_correlated_tbl %>%
  plot_correlation_funnel(interactive = TRUE)

marketing_campaign_correlated_tbl %>%
  filter(feature %in% c("DURATION", "POUTCOME", "PDAYS", 
                        "PREVIOUS", "CONTACT", "HOUSING")) %>%
  plot_correlation_funnel(interactive = FALSE, limits = c(-0.4, 0.4))
