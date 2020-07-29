library(data.table)

hawks <- fread('https://vincentarelbundock.github.io/Rdatasets/csv/Stat2Data/Hawks.csv', 
               select = c('Species', 'Age', 'Sex', 'Wing', 'Weight', 'Tail', 'KeelFat', 'Tarsus', 'Year'))

hawks[, Month := NULL]
colnames(hawks)

# View the first five rows
hawks[1:5]

hawks[, Over1000Weight := Weight > 1000]
hawks[, .(Weight, Over1000Weight)]
colnames(hawks)

hawks[is.na(Weight)]
hawks[!complete.cases(hawks)]

# of nas in column
hawks[is.na(KeelFat), .N]
hawks[is.na(Tarsus), .N]

# drop columns
hawks[, c("KeelFat", "Tarsus") := NULL]

# No need to reassign
hawks[1:5]

# incomplete cases
hawks[!complete.cases(hawks)]

hawks <- hawks[complete.cases(hawks)]
hawks[is.na(Weight)]

# check for a blank string
hawks[, unique(Sex)]

# see how many
hawks[, .N, by = Sex]

hawks[Sex == "", Sex := "Unknown"]
hawks[1:5]

hawks[, .N, by = Species]
hawks[, .N, by = Age]

# change values
hawks[Sex == "F", Sex := "Female"]
hawks[Sex == "M", Sex := "Male"]

# set factors
hawks[, Sex := factor(Sex, levels = c("Female", "Male", "Unknown"))]
levels(hawks[, Sex])

hawks[, Species := factor(Species, levels = c("CH", "RT", "SS"))]
levels(hawks$Species) <- c("Cooper's", "Red-tailed", "Sharp-shinned")

levels(hawks[, Species])

# grouping and counting
# Filter by year, then count cases while grouping by species and age
age_species_2000 <- hawks[Year == 2000, .(Count = .N), by = .(Species, Age)]
age_species_2000

# Filter to adults
adult_species_2000 <- age_species_2000[Age == "A"]
adult_species_2000

# Order by count
ordered <- adult_species_2000[order(-Count)]
ordered

# Select the value for the species in the first row
most_adults <- ordered[1, Species]
most_adults
# chain it all together
hawks[Year == 2000, .(Count = .N), by = .(Species, Age)][Age == "A"][order(-Count)][1, Species]
