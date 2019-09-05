# load the blastula package
library("blastula")

# create a simple e-mail
email <- compose_email(body = "Insert your e-mail body here",
                       footer = "Insert your e-mail footer here")

# preview e-mail in Viewer pane
preview_email(email)

# create an e-mail with R code

email_body <- 
  "
Hi! This new report was generated at {Sys.time()}
"

email_footer <- 
  "
Please contact *support@acme.com* with any questions
"

email <- compose_email(body = email_body,
                       footer = email_footer)

# preview e-mail in Viewer pane
preview_email(email)


# create an e-mail with a plot
library(ggplot2)
plot <- ggplot(mtcars, aes(cyl, carb)) + geom_bar(stat = "identity")


email_body <- 
  "
Hi! This new report was generated at {Sys.time()} \\


{add_ggplot(plot, width = 5, height = 3)}

"

email_footer <- 
  "
Please contact *support@acme.com* with any questions
"

email <- compose_email(body = email_body,
                       footer = email_footer)

# preview e-mail in Viewer pane
preview_email(email)
