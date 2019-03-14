#title: shots-data
#description: shots data of the players
#input: stephen-curry.csv, andre-iguodala.csv, kevin-durant.csv, klay-thompson.csv, draymond-green.csv
#output: stephen-curry-summary.txt, andre-iguodala-summary.txt, kevin-durant-summary.txt, klay-thompson-summary.txt, draymond-green-summary.txt, shots-data.csv, shots-data-summary.txt

# Read in the five data sets
curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)

# Add a column name to each imported data frame
curry$name <- "Stephen Curry"
iguodala$name <- "Andre Iguodala"
durant$name <- "Kevin Durant"
thompson$name <- "Klay Thompson"
green$name <- "Draymond Green"

# Change the original values of shot_made_flag to more descriptive values
curry$shot_made_flag[curry$shot_made_flag == "n"] <- "shot_no"
curry$shot_made_flag[curry$shot_made_flag == "y"] <- "shot_yes"

iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] <- "shot_no"
iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] <- "shot_yes"

durant$shot_made_flag[durant$shot_made_flag == "n"] <- "shot_no"
durant$shot_made_flag[durant$shot_made_flag == "y"] <- "shot_yes"

thompson$shot_made_flag[thompson$shot_made_flag == "n"] <- "shot_no"
thompson$shot_made_flag[thompson$shot_made_flag == "y"] <- "shot_yes"

green$shot_made_flag[green$shot_made_flag == "n"] <- "shot_no"
green$shot_made_flag[green$shot_made_flag == "y"] <- "shot_yes"

# Add a column minute that contains the minute number where a shot occurred
curry$minute <- (curry$period) * 12 - curry$minutes_remaining
iguodala$minute <- (iguodala$period) * 12 - iguodala$minutes_remaining
durant$minute <- (durant$period) * 12 - durant$minutes_remaining
thompson$minute <- (thompson$period) * 12 - thompson$minutes_remaining
green$minute <- (green$period) * 12 - green$minutes_remaining

# Use sink() to send summary() output of each imported date frame into individuals text files
sink(file="../output/stephen-curry-summary.txt")
summary(curry)
sink()

sink(file="../output/andre-iguodala-summary.txt")
summary(iguodala)
sink()

sink(file="../output/kevin-durant-summary.txt")
summary(durant)
sink()

sink(file="../output/klay-thompson-summary.txt")
summary(thompson)
sink()

sink(file="../output/draymond-green-summary.txt")
summary(green)
sink()

# Use row binding function rbind() to stack the tables into one single data frame
shots_data <- rbind(curry,iguodala,durant,thompson,green)

# Export the assembled table as a CSV-file shots-data inside the folder data/.
write.csv(shots_data,"../data/shots-data.csv")

# Use sink() to send the summary() output of the assembled table to a text file inside output/folder
sink(file="../output/shots-data-summary.txt")
summary(shots_data)
sink()


