# install.packages("RDCOMClient", repos = "http://www.omegahat.net/R")
library(RDCOMClient)
setwd("~/Data/Datasets")
mail_list <- read.csv("mail_list.csv", header=TRUE, stringsAsFactors = FALSE)
mail_list
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
  Email[["htmlbody"]] =
  "<h1>This is a test</h1>
  <p>Link to google/bookmarks</p>
  <a href='https://www.w3schools.com/html/'>Visit our HTML tutorial</a>
  <img src='http://www.resultspad.net/pexels-photo-1036623.jpeg'>
"

# Send the message
Email$Send()
}
# Close Outlook, clear the message
rm(Outlook, Email)

# Email[["SendUsingAccount"]]="myemail@email.com"

