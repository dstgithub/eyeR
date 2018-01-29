##### Code 1. Set working directory and import data using read.table()

# set working directory, which is the folder containing the files and sub-folders
setwd("C:/eyeR")

# import grade and eye tracking data
data.df <- read.table("data/eye-tracking-numeric-data.txt", header=TRUE)
head(data.df)  # display the first 6 rows


##### Code 2. Select observations (rows) from a data frame based on variable values

data1.df <- data.df[data.df$grade == 1, ]	


##### Code 3. Export data to a .txt file

write.table(data1.df, "work/eye-tracking-numeric-data1.txt", row.names = FALSE)


##### Code 4. Correlation coefficient between two variables

fixation_duration <- c(49.88, 166.23, 75.33, 49.08, 69.47, 30.7)
fixation_count <- c(162, 533, 224, 151, 258, 126)
cor(fixation_duration, fixation_count)


##### Code 5. Correlation matrix of fixation duration, fixation count and visit count in each of the three questions. 

# apply cor() to the last 9 columns of data.df,
# and then round the values to 3 decimal places
r1 <- round(cor(data.df[, 2:10]), 3)
r1


##### Code 6. Logistic regression showing relationship between grade and fixation duration in each problem

logt <- glm(grade ~ q1fd + q2fd + q3fd, data = data.df, family=binomial(link="logit"))
summary(logt)


##### Code 7. A for loop to calculate the mean value of each variable (column)

col_ave <- rep(NA, length = ncol(data.df))  # initiate vector col_ave
for (i in 1:10){
  col_ave[i] <- mean(data.df[, i])
}
col_ave


##### Code 8.  Apply the mean() function over every column of data.df using apply()

apply(data.df, 2, mean)  # "2" for column


##### Code 9. Import AOI files to a list 

# set working directory containing the aoi files 
setwd("C:/eyeR/txt-aoi")

# store all the 81 AOI file names in "filenames"
filenames <- list.files(full.names = TRUE)

# import the 81 AOI files and store in list "aoiFiles"
aoiFiles <- lapply(filenames, function(x){
  read.table(x, header = TRUE, sep = "\t", skip = 11)
})


##### Code 10. Extract AOIs for Item 11

# extract rows with "tor11.html" in column "StimuliName" and
# store the 1st and 4th columns in aoiFiles11
aoiFiles11 <- lapply(aoiFiles, function(x){
  x[grep("tor11.html", x$StimuliName), c(1,4)]
})

# check the 1st three rows of the 1st data frame in aoiFiles11
head(aoiFiles11[[1]], 3)


##### Code 11. Obtain and export AOIs in Phase 2

## Find difference between the last and 1st timestamps (in ms)

# number of rows of each data frame
nrow_q11 <- lapply(aoiFiles11, function(x) nrow(x))

# difference between the last ([nrow_q11[[i]], 1]) and 1st ([1, 1]) timestamps in ith # data frame: 
dif_q11 <- lapply(1:81, function(i) {
  aoiFiles11[[i]][nrow_q11[[i]], 1] - aoiFiles11[[i]][1, 1]
})

## range of timestamps for Phase 2

# define the range of percentages for Phase 2
p2 <- c(0.3, 0.91)

# actual timestamp range for Phase 2
tstamp <- lapply(1:81, function(i) {
  dif_q11[[i]] * p2 + aoiFiles11[[i]][1, 1]
})

tstamp[[1]] #  check the first timestamp pairs for Phase 2

## Obtain AOIs between tstamp[[i]][2] and tstamp[[i]][1]

# define function for subset of data frame aoiFiles11 in Phase 2
aoi_phase2 <- function(x, y){
  subset(x, x$Timestamp > y[1] & x$Timestamp < y[2]) 
}

# apply the above function (use mapply since there are 2 arguments in aoi_phase2())
q11_phase2 <- mapply(aoi_phase2, x = aoiFiles11, y = tstamp, SIMPLIFY = FALSE)

# get AOI columns only, and then convert to character
q11_aoi_phase2 <- lapply(q11_phase2, "[[", 2)
q11_aoi_proc_phase2 <- lapply(q11_aoi_phase2, as.character)

## Export AOIs in Phase 2 of Item 11 for all the 81 recordings

# each row is for a recording
setwd("C:/eyeR")
lapply(q11_aoi_proc_phase2, write, "work/q11_aoi_proc_phase2.txt", sep = "\t",
       append = TRUE, ncolumns = 1000)


##### Code 12. Import conversion key and convert AOIs to scanpaths

# import conversion key
conv.df <- read.table("file/q11-conversion-key.txt", header = TRUE)
# check the first 3 rows of conv.df
head(conv.df, 3)

# install and load package GrpString
install.packages("GrpString") # if the package has not been installed
library(GrpString)

# convert AOIs to scanpaths using function EveString()
s2 <- EveString("output/q11_aoi_proc_phase2.txt", conv.df$aoi, conv.df$code)
head(s2, 3)


##### Code 13. Process scanpaths: removed certain characters and obtain scanpaths for grade = 1

# remove AOI-"Content" (code: "0")
s2_no0 <- gsub("0","", s2)

# apply function DupRm() to remove successive repeated AOIs
s2x <- DupRm(s2_no0) 
head(s2x, 3)

# combine grade and scanpath vectors
s2x.df <- cbind(data.df$grade, s2x)

# separate scanpaths with grade = 1
s2x1 <- s2x.df[s2x.df[,1] == "1", 2]
length(s2x1) # check the number of scanpaths in vector s2x1


##### Code 14. Using function CommonPatt() to find patterns in a group of scanpaths.

ptn1.df <- CommonPatt(s2x1, low = 20)
ptn1.df


##### Code 15. Generate word cloud to visualize scanpath patterns and frequencies

install.packages("wordcloud")
library(wordcloud)
png("work/wordcloud.png", width = 500, height = 470)
if(require(RColorBrewer)){
  pal <- brewer.pal(6,"Dark2")
  pal <- pal[-(1)]
  wordcloud(ptn1.df$Pattern, ptn1.df$Freq_grp,c(6,.3),2,,TRUE,TRUE,.1,pal)
}
dev.off()


