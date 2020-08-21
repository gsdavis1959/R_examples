devtools::install_github("haozhu233/kableExtra")
library(knitr)
library(kableExtra)
dt <- mtcars[1:5, 1:6]

kbl(dt)

dt %>%
  kbl() %>%
  kable_styling()

dt %>%
  kbl() %>%
  kable_paper("hover")

dt %>%
  kbl(caption = "Recreating booktabs style table") %>%
  kable_classic(full_width = F, html_font = "Calibri")

dt %>%
  kbl() %>%
  kable_classic_2()

dt %>%
  kbl() %>%
  kable_minimal()

dt %>%
  kbl() %>%
  kable_material(c("striped", "hover"))

dt %>%
  kbl() %>%
  kable_material_dark("hover")

