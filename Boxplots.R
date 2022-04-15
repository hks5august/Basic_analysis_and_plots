#Import the data
df <- read.table('https://raw.githubusercontent.com/pine-bio-support/DataScience/main/Final_cell_lines_RNA-expression_FPKM_values_1000genes_with_NA.txt', sep ='\t', header = TRUE, row.names=1)

#Remove NA values
df1 <-  na.exclude(df)

#Perform log-scale transformation of the data
log_df <- log(df1+1)

#Check the dimension of data
dim(log_df)

#Check the summary statistics of data after log transformation
summ_log <- summary(log_df)

#Write summary statistics to a file and export it
write.table(summ_log, file="stat_sum-log-data.txt", col.names=TRUE, sep="\t")

#Draw boxplot for log-trandformed data
boxplot(log_df, main="Boxplot for log-transformed Data", xlab="", ylab="Gene expression (log[FPKM])", col="red", las=2, cex.axis = 0.7)

#Check boxplot 
print(length(boxplot(log_df, main="Boxplot for log-transformed Data", xlab="", ylab="Gene expression (log[FPKM])", col="red", las=2, cex.axis = 0.7)))

#Import the data
df <- read.table('https://raw.githubusercontent.com/pine-bio-support/DataScience/main/Final_cell_lines_RNA-expression_FPKM_values_1000genes_with_NA.txt', sep ='\t', header = TRUE, row.names=1)

#Remove NA values
df1 <-  na.exclude(df)

#Perform log-scale transformation of the data
log_df <- log(df1+1)

# Extract the  samples from the groups using the name of group
Non_ML1 <- as.data.frame(log_df[grep('^Non.malignant', names(log_df))])
Claudin1 <- as.data.frame(log_df[grep('^Claudin', names(log_df))])

#Compute the mean for each gene within each group
Non_ML<- rowMeans(Non_ML1)
Claudin <- rowMeans(Claudin1)

#Bind groups together into 2 columns
group <- cbind(Non_ML, Claudin)

#Provide the column names to both groups
colnames(group) <- c("Non-malignant", "Claudin-low")

#Draw boxplot for groups of samples
boxplot(group, main="Boxplot for groups", xlab="Groups", ylab="Gene expression(log[FPKM])", col=c('cyan', 'pink'))
