library(tidyverse)

model <- list(
  "hello" = c(
    "How do you do. Please state your problem."
  ),
  "computer" = c(
    "Do computers worry you?",
    "What do you think about machines?",
    "Why do you mention computers?",
    "What do you think machines have to do with your problem?"
  ),
  "name" = c(
    "I am not interested in names"
  ),
  "sorry" = c(
    "Please don't apologize",
    "Apologies are not necessary",
    "What feelings do you have when you apologize"
  ),
  "I remember" = c(
    "Do you often think of $?",
    "Does thinking of $ bring anything else to mind?",
    "What else do you remember?",
    "Why do you recall $ right now?",
    "What in the present situation reminds you of $?",
    "What is the connection between me and $?"
  ),
  "do you remember" = c(
    "Did you think I would forget $?",
    "Why do you think I should recall $ now?",
    "What about $?",
    "You mentioned $"
  ),
  "I want" = c(
    "What would it mean if you got $?",
    "Why do you want $?",
    "Suppose you got $ soon."
  ),
  "I dreamt" = c(
    "How do you feel about $ in reality?"
  ),
  "dream" = c(
    "What does this dream suggest to you?",
    "Do you dream often?",
    "What persons appear in your dreams?",
    "Don't you believe that dream has to do with your problem?"
  ),
  "my mother" = c(
    "Tell me more about your family"
  ),
  "my father" = c(
    "Your father?",
    "Does he influence you strongly?",
    "What else comes to mind when you think of your father?"
  ),
  "I am glad" = c(
    "How have I helped you to be $?",
    "What makes you happy just now?",
    "Can you explain why you are suddenly $?"
  ),
  "I am sad" = c(
    "I am sorry to hear you are depressed",
    "I'm sure it's not pleasant to be sad"
  ),
  "alike" = c(
    "In what way?",
    "What similarities are there?"
  ),
  "same" = c(
    "What other connections do you see?"
  ),
  "no" = c(
    "Why not?",
    "You are being a bit negative.",
    "Are you saying 'No' just to be negative?"
  ),
  "I was" = c(
    "Were you really?",
    "Perhaps I already knew you were $.",
    "Why do you tell me you were $ now?"
  ),
  "was I" = c(
    "What if you were $?",
    "Do you think you were $?",
    "What would it mean if you were $?"
  ),
  "I am" = c(
    "In what way are you $?",
    "Do you want to be $?"
  ),
  "am I" = c(
    "Do you believe you are $?",
    "Would you want to be $?",
    "You wish I would tell you you are $?",
    "What would it mean if you were $?"
  ),
  "are you" = c(
    "Why are you interested in whether I am $ or not?",
    "Would you prefer if I weren't $?",
    "Perhaps I am $ in your fantasies"
  ),
  "you are" = c(
    "What makes you think I am $?"
  ),
  "because" = c(
    "Is that the real reason?",
    "What other reasons might there be?",
    "Does that reason seem to explain anything else?"
  ),
  "were you" = c(
    "Perhaps I was $?",
    "What do you think?",
    "What if I had been $?"
  ),
  "I can't" = c(
    "Maybe you could $ now",
    "What if you could $?"
  ),
  "I feel" = c(
    "Do you often feel $?"
  ),
  "I felt" = c(
    "What other feelings do you have?"
  ),
  "why don't you" = c(
    "Should you $ yourself?",
    "Do you believe I don't $?",
    "Perhaps I will $ in good time"
  ),
  "yes" = c(
    "You seem quite positive",
    "You are sure?",
    "I understand"
  ),
  "somebody" = c(
    "Can you be more specific?"
  ),
  "everybody" = c(
    "Surely not everyone",
    "Can you think of anyone in particular?",
    "Who, for example?",
    "You are thinking of a special person"
  ),
  "always" = c(
    "Can you think of a specific example?",
    "When?",
    "What incident are you thinking of?",
    "Really--always?"
  ),
  "what" = c(
    "Why do you ask?",
    "Does that question interest you?",
    "What is it you really want to know?",
    "What do you think?",
    "What comes to your mind when you ask that?"
  ),
  "perhaps" = c(
    "You do not seem quite certain"
  ),
  "are" = c(
    "Did you think they might not be $?",
    "Possibly they are $"
  )
)

default_model <- c(
  "Very interesting",
  "I am not sure I understand you fully",
  "What does that suggest to you?",
  "Please continue",
  "Go on",
  "Do you feel strongly about discussing such things?"
)


Eliza <- function(input) {
  # match keywords from model
  pos <- which(lapply(paste0("(.*)?", names(model), "(.*)?"), grep, x = input, ignore.case = TRUE) == 1)
  output <- unlist(model[pos])
  if (length(pos) == 0) {
    # choose default answer randomly if no keyword is found
    output <- sample(default_model, 1)
  } else {
    # choose applicable answer randomly
    pos <- ifelse(length (pos) > 1, sample(pos, 1), pos)
    output <- sample(output, 1)
    names(output) <- NULL
    # customize answer
    tmp <- regexec(names(model)[pos], input, ignore.case = TRUE)[[1]]
    end_phrase <- substr(input, start = attr(tmp, "match.length") + as.numeric(tmp) + 1, stop = nchar(input))
    end_phrase <- trimws(end_phrase, which = "right", whitespace = "[?!.]")
    output <- sub("\\$", end_phrase, output)
  }
  output
}

input <- ""
cat("Eliza: Hello, I am Eliza!\n")

while (TRUE) {
  input <- readline("You: ")
  if (input == "quit") break
  cat("Eliza:", Eliza(input))
}
