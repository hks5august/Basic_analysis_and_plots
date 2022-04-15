#Import the data
df <- read.table('https://raw.githubusercontent.com/pine-bio-support/DataScience/main/Final_cell_lines_RNA-expression_FPKM_values_1000genes_with_NA.txt', sep ='\t', header = TRUE, row.names=1)

#Remove NA values
df1 <-  na.exclude(df)

#Perform log-scale transformation of the data
log_df <- log(df1+1)

#Convert dataframe (with log values) into a matrix
log_df1 <- as.matrix(log_df) 

#Draw histogram, breaks represents the bins 
hist(log_df1,  main= "Histogram for all samples" , xlab= "samples" , breaks= 80 , col=rainbow( 11 ))

#Extract desired  samples, here, we are extracting sample 1 and sample 8 
sample1 <- log_df1[, 1 ] 
sample2 <- log_df1[, 8 ] 

#Define colors for histogram 
col1 <- rgb( 0 , 0 , 1 , 0.5 ) 
col2 <- rgb( 1 , 0 , 0 , 0.5 ) 

#Draw histograms 
hist(sample1, col=col1, main = "Histograms of two samples" , xlab= "" , breaks= 200 ) 
hist(sample2, col=col2, main= "" , xlab= "" , breaks= 200 , add=T)


###### Desity plot #########

#Add density for each sample 
d1 <- density(sample1) 
d2 <- density(sample2) 

#Plot Density plot 
plot(d2, col= "red" ) 
polygon(d2, col=col2, border=col2) 
lines(d1, col= "blue" ) 
polygon(d1, col=col1, border=col1)

