devtools::install_github("joachim-gassen/ExPanDaR")
library(ExPanDaR)
ExPanD(df = russell_3000,  
       df_def = russell_3000_data_def, 
       df_name = "Russell 3000",
       config_list = ExPanD_config_russell_3000,
       export_nb_option = TRUE)
