#Load Libraries
library(dplyr)
library(ggplot2)
library(reshape2)

# Load input data

#data <-  read.xlsx("17sample-OTU-mat1_phylum1.xlsx", sheet = 2)

data <- read.table("example_tpose.csv", sep=",",header=T )
# Load ID that to match 
ids <- read.table("samples-ids1", sep=",",header=T )
#ids

df=as.data.frame(data)

### Extract Matching IDs (Rows) from the the file
df2 <- df[ df$ID %in% c(ids$ID), ]

### Write as file
write.table(df2, file="out2.csv", sep=",", row.names = F, )
 