# from:
# How to calculate accuracy_group with event_data
# Created by Guy Zahavi October 30 2019

library(tidyverse)
library(RColorBrewer)
library(jsonlite)

labels <- read.csv('train_labels.csv', stringsAsFactors = FALSE)
train <- read.csv('train.csv', stringsAsFactors = FALSE)
specs <- read.csv('specs.csv', stringsAsFactors = FALSE)
test <- read.csv('test.csv', stringsAsFactors = FALSE)

# Cart Balancer
specs %>% filter(event_id == 'd122731b') %>% select(-args) 

# Bird Measurer
specs %>% filter(event_id == '17113b36')%>% select(-args) 

# Cauldron Filler
specs %>% filter(event_id == '392e14df') %>% select(-args) 

# Mushroom Sorter
specs %>% filter(event_id == '25fa8af4') %>% select(-args) 

# Chest Sorter
specs %>% filter(event_id == '93b353f2') %>% select(-args)


# Keep only necessary events for accuracy evaluation
events_to_keep <- c('d122731b', '17113b36', '392e14df', '25fa8af4', '93b353f2')
assessments <- train %>% filter(type == 'Assessment') 

dim(assessments)
head(assessments)
tail(assessments)


assessments_with_events <- assessments %>% filter(event_id %in% events_to_keep) %>% select(game_session) %>% unique()
unlabeled_events <- setdiff(labels$game_session, unlist(assessments_with_events, use.names = F))
print(paste('There are', length(unlist(assessments_with_events, use.names = F)), 'assessments with these event ids in the train data'))
print(paste('There are', length(labels$game_session), 'assessments in the train_labels.csv file'))
print(paste('There is a difference of', length(unlabeled_events), 'assessments'))

rm(assessments_with_events, unlabeled_events)


# Scoring a single assessment from the train data, includes a comparison to train_labels.csv -----------------------
score_assessment <- function(assessment_data) {
  game_labels <- labels %>% filter(game_session == assessment_data$game_session[1]) %>%
    select(-installation_id, -title)
  
  # Read events data
  score_events <- assessment_data %>% filter(event_id %in% events_to_keep)
  
  if (nrow(score_events) == 0) {
    results <- data.frame(game_session = assessment_data$game_session[1],
                          installation_id = assessment_data$installation_id[1],
                          title = assessment_data$title[1],
                          my_correct = NA,
                          my_incorrect = NA,
                          my_accuracy = NA,
                          my_accuracy_group = NA)
  }
  else {
    score_json <- lapply(score_events$event_data, fromJSON)
    
    ### Score
    # Calculating score
    scores <- lapply(score_json, '[', 'correct') %>% unlist()
    accuracy <- mean(scores)
    correct <- sum(scores)
    incorrect <- sum(!scores)
    
    accuracy_group <- case_when(
      accuracy == 0 ~ 0,
      accuracy == 1 ~ 3,
      accuracy >= 0.5 ~ 2,
      accuracy < 0.5 ~ 1
    )
    
    results <- data.frame(game_session = assessment_data$game_session[1],
                          installation_id = assessment_data$installation_id[1],
                          title = assessment_data$title[1],
                          my_correct = correct,
                          my_incorrect = incorrect,
                          my_accuracy = accuracy,
                          my_accuracy_group = accuracy_group)
    
  }
  
  suppressWarnings(results <- left_join(results, game_labels, by = 'game_session'))
  results$accuracy_correct <- (results$my_accuracy == results$accuracy)
  results$group_correct <- (results$my_accuracy_group == results$accuracy_group)
  
  return(results)  
}
# Score a single assessment
assessment_ids <- assessments %>% select(game_session) %>% unique()
dim(assessment_ids)
str(assessment_ids)
score_assessment(assessments %>% filter(game_session == unlist(assessment_ids[1,1])))
score_assessment(assessments %>% filter(game_session == unlist(assessment_ids[10,1])))


# There are 21239 assessments, let's split them to 5 seperate computations to prevent the session from crashing
split_assessment_labels <- apply(assessment_ids[1:4000,1, drop=F], 1, function(i) score_assessment(filter(assessments,
                                                                                                  game_session == i)))
assessment_labels <- do.call(rbind.data.frame, split_assessment_labels)

# part 2/5
split_assessment_labels <- apply(assessment_ids[4001:8000,1, drop=F], 1, function(i) score_assessment(filter(assessments,
                                                                                                     game_session == i)))
df <- do.call(rbind.data.frame, split_assessment_labels)

assessment_labels <- rbind(assessment_labels, df)

# part 3/5
split_assessment_labels <- apply(assessment_ids[8001:12000,1, drop=F], 1, function(i) score_assessment(filter(assessments,
                                                                                                      game_session == i)))
df <- do.call(rbind.data.frame, split_assessment_labels)

assessment_labels <- rbind(assessment_labels, df)

# part 4/5
split_assessment_labels <- apply(assessment_ids[12001:16500,1, drop=F], 1, function(i) score_assessment(filter(assessments,
                                                                                                       game_session == i)))
df <- do.call(rbind.data.frame, split_assessment_labels)

assessment_labels <- rbind(assessment_labels, df)

# part 5/5
split_assessment_labels <- apply(assessment_ids[16501:nrow(assessment_ids),1, drop=F], 1, function(i) score_assessment(filter(assessments,
                                                                                                                      game_session == i)))
df <- do.call(rbind.data.frame, split_assessment_labels)

assessment_labels <- rbind(assessment_labels, df) 

print(paste('A total of', nrow(assessment_labels), 'assessments were compared'))
print(paste('Accuracy is ', mean(assessment_labels$accuracy_correct, na.rm = TRUE)*100, '% identical', sep = ''))
print(paste('Accuracy group is ', mean(assessment_labels$group_correct, na.rm = TRUE)*100, '% identical', sep = ''))

head(assessment_labels)

# test data
events_to_keep <- c('d122731b', '17113b36', '392e14df', '25fa8af4', '93b353f2')
assessments_test <- test %>% filter(type == 'Assessment') 

score_test_assessment <- function(assessment_data) {
  
  # Read events data
  score_events <- assessment_data %>% filter(event_id %in% events_to_keep)
  
  if (nrow(score_events) == 0) {
    results <- data.frame(game_session = assessment_data$game_session[1],
                          installation_id = assessment_data$installation_id[1],
                          title = assessment_data$title[1],
                          correct = NA,
                          incorrect = NA,
                          accuracy = NA,
                          accuracy_group = NA)
  }
  else {
    score_json <- lapply(score_events$event_data, fromJSON)
    
    ### Score
    # Calculating score
    scores <- lapply(score_json, '[', 'correct') %>% unlist()
    accuracy <- mean(scores)
    correct <- sum(scores)
    incorrect <- sum(!scores)
    
    accuracy_group <- case_when(
      accuracy == 0 ~ 0,
      accuracy == 1 ~ 3,
      accuracy >= 0.5 ~ 2,
      accuracy < 0.5 ~ 1
    )
    
    results <- data.frame(game_session = assessment_data$game_session[1],
                          installation_id = assessment_data$installation_id[1],
                          title = assessment_data$title[1],
                          correct = correct,
                          incorrect = incorrect,
                          accuracy = accuracy,
                          accuracy_group = accuracy_group)
    
  }
  
  return(results)  
}
# Score a single assessment
assessment_test_ids <- assessments_test %>% select(game_session) %>% unique()
dim(assessment_ids)
score_test_assessment(assessments_test %>% filter(game_session == unlist(assessment_test_ids[1,1])))
score_test_assessment(assessments_test %>% filter(game_session == unlist(assessment_test_ids[10,1])))

list_assessment_labels <- apply(assessment_test_ids, 1, 
                                function(i) score_test_assessment(filter(assessments_test,
                                                                         game_session == i)))

assessment_labels <- do.call(rbind.data.frame, list_assessment_labels)

print(paste('A total of', nrow(assessment_labels), 'assessments were evaluated'))

head(assessment_labels)

test_ids <- test %>% select(installation_id) %>% unique()
dim(test_ids)

# See if there are any id's without assessments
length(setdiff(unlist(test_ids, use.names = F), unlist(assessments_test$installation_id, use.names = F)))

# Find the most common accuracy group for each installation_id
most_common_group <- apply(test_ids, 1, function(x) 
  names(which.max(table(select(filter(assessment_labels, installation_id == x), accuracy_group)))))

head(most_common_group)

# Save submission file
submission <- data.frame(
  installation_id = test_ids[,1],
  accuracy_group = as.character(most_common_group))
# Replace NULLs with zeros
submission$accuracy_group[which(submission$accuracy_group == 'NULL')] <- '0'



