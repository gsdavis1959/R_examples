# install.packages("RDCOMClient", repos = "http://www.omegahat.net/R")
library(RDCOMClient)
setwd("~/Data/Datasets")
mail_list <- read.csv("mail_list.csv", header=TRUE, stringsAsFactors = FALSE)
mail_list

library(readr)

body <- read_file("html_body.txt")
body
# Open Outlook
Outlook <- COMCreate("Outlook.Application")

# Set the recipient, subject, and body
for (recipient in 1:nrow(mail_list)) {
  Email = Outlook$CreateItem(0)
  Email[["to"]] = (mail_list$To[recipient])
  Email[["cc"]] = (mail_list$cc[recipient])
# Send the message from an alternate account
  Email[["SentOnBehalfOfName"]] = ""
  Email[["bcc"]] = ""
  Email[["subject"]] = "Test"
  Email[["htmlbody"]] = body
 
# Send the message
Email$Send()
print(mail_list$To[recipient])
}
# Close Outlook, clear the message
rm(Outlook, Email)

# Email[["SendUsingAccount"]]="myemail@email.com"

