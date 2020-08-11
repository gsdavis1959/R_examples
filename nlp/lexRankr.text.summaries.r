#load needed packages
library(xml2)
library(rvest)
library(lexRankr)

#url to scrape
monsanto_url = "https://www.theguardian.com/environment/2017/sep/28/monsanto-banned-from-european-parliament"

#read page html
page = xml2::read_html(monsanto_url)
#extract text from page html using selector
page_text = rvest::html_text(rvest::html_nodes(page, ".js-article__body p"))
print(page_text)
#perform lexrank for top 3 sentences
top_3 = lexRankr::lexRank(page_text,
                          #only 1 article; repeat same docid for all of input vector
                          docId = rep(1, length(page_text)),
                          #return 3 sentences to mimick /u/autotldr's output
                          n = 3,
                          continuous = TRUE)

#reorder the top 3 sentences to be in order of appearance in article
order_of_appearance = order(as.integer(gsub("_","",top_3$sentenceId)))
#extract sentences in order of appearance
ordered_top_3 = top_3[order_of_appearance, "sentence"]
head(ordered_top_3)
str(page_text)
df <- unlist(page_text)
df
sentenceSimil(docId=c("d1","d1","d2","d2"),
               sentenceId=c("d1_1","d1_1","d2_1","d2_1"),
               token=c("Monsanto lobbyists have been banned from entering the European parliament after the multinational refused to at", 
                       "Monsanto lobbyists have been banned from entering the European parliament after the multinational refused to attend a parliamentary hearing into allegations of regulatory interference",
                       "It is the first time MEPs have used new rules to withdraw parliamentary access for firms that ignore a summons to attend parliamentary inquiries or hearings.",
                       "Monsanto officials will now be unable to meet MEPs, attend committee meetings or use digital resources on parliament premises in Brussels or Strasbourg."))
