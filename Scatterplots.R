#Import the data
df <- read.table('https://raw.githubusercontent.com/pine-bio-support/DataScience/main/Final_cell_lines_RNA-expression_FPKM_values_1000genes_with_NA.txt', sep ='\t', header = TRUE, row.names=1)

#Remove NA values
df1 <-  na.exclude(df)


#Extract samples of same group
sample1 <- df1[,1]
sample2 <- df1[,2]

#Combine them column-wise
s1_2 <- cbind(sample1,sample2)

#Draw Scatterplot
plot(s1_2)


#Extract samples of different groups
sample3 <- df1[,1]
sample4 <- df1[,8]

#Combine them column-wise
s3_4 <- cbind(sample3,sample4)

#Draw Scatterplot
plot(s3_4)

#Print dimension of object used for scatterplot
print(dim(s3_4))