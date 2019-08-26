data(mtcars)
head(mtcars)

lm(mpg ~ hp, data = mtcars)$coefficients

response_list <- c("mpg", "disp", "drat")

# ways to change response variables to analyze
# 1. as.formula()

for (y in response_list) {
  lmfit <- lm(as.formula(paste(y, "~ hp")), data = mtcars)
  print(lmfit$coefficients)
}

# 2. Don’t specify the data option
for (y in response_list) {
  lmfit <- lm(mtcars[[y]] ~ mtcars$hp) 
  print(lmfit$coefficients)
} 

# 3. get()
for (y in response_list) {
  lmfit <- lm(get(y) ~ hp, data = mtcars)
  print(lmfit$coefficients)
}

# 4. eval(parse())
for (y in response_list) {
  lmfit <- lm(eval(parse(text = y)) ~ hp, data = mtcars)
  print(lmfit$coefficients)
}
