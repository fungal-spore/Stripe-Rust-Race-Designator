# Stripe Rust Race Designator
# Purpose of this script is to compare differential set data with database of existing races
setwd("C:/User/StripeRust/differential-testing")

# Import existing race file
racedb <- read.table("C:/User/StripeRust/2026_Pst-races-database.csv", header=TRUE, sep=",")
# Cut file to have just race names and virluence gene list
races <- racedb[,1:3]

# Import differential data set with columns for each diffential line: Isolate, Yr1, Yr5, etc.
# Data should be: v = virulent or a = avirulent for each differential, see example data file
data <- read.table("C:/User/StripeRust/differential-testing/test-data.csv", header=TRUE, sep=",")
# Create dataframe for outputing results
df <- as.data.frame(data$Isolate)

# Collapse virulence profiles (a's and v's) into a single column, rather than
# searching many columns for matches, now only need to match single strings
# from https://stackoverflow.com/questions/47763452/r-collapsing-data-from-multiple-columns-into-one
df$profile <- apply(data[,2:19], 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = ""))
races$profile <- apply(racedb[,4:21], 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = ""))

# Create new columns in the results file
df$CAN <- NA
df$US <- NA
df$Virulence <- NA

# Loop which lasts for as many rows as are in the the df file
for (i in seq_len(nrow(df))) {
  match_index <- which(races$profile == df$profile[i]) #find match profile in races, the [i] for df$profile should increase by 1 for each loop and therefor go to next row
  #the above match_index will be a row number from the races file, and so will 'hit' if it is greater than 0  
  if (length(match_index) > 0) {
    df$CAN[i] <- races[match_index[1], 1] #prints Canadian race info from the 'hit' row into results 
    df$US[i] <- races[match_index[1], 2]  #prints US race info from the 'hit' row into results
    df$Virulence[i] <- races[match_index[1], 3]  #prints virulence list from the 'hit' row into results
  } # If there is no match in the races file will leave everything blank, no need for 'else'
}

# Create and print output file to working directory
outputfile<- subset(df, select = -profile) # Drops the av profile column
colnames(outputfile)[1]<- "Isolate"
write.table(outputfile, "RaceDesignations.tsv", sep = "\t", row.names=FALSE, quote=FALSE) 
