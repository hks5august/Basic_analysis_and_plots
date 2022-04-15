#Load libraries 
library(ggplot2) 
library(reshape2)

#import Data 
df <- read.table("https://raw.githubusercontent.com/pine-bio-support/DataScience/main/Final_cell_lines_RNA-expression_FPKM_values_1000genes_with_NA.txt", header=TRUE,  sep="\t", row.names= 1)

#Remove "NA" values from the data 
df1 <- na.exclude(df) 

#Transform FPKM values into log values 
log_df <- log(df1+1)

#Reshape or transform data into a long data frame format 
df_log <- melt(log_df)

#Draw boxplot 
p <- ggplot(df_log, aes(factor(variable), value))  + geom_boxplot(aes(fill = variable)) 

#Print length of plot object
print(length(p))

#Add axis title, legend title and adjust font size 
p + theme(legend.key.size = unit(0.9, "cm"), legend.text = element_text(color= "Black", size= 9), axis.text.x = element_text(color= "Black", size= 9, angle= 90), axis.text.y = element_text(size= 10, angle= 45)) + xlab("Samples") + ylab("Gene Expression (Log[FPKM])") + labs(fill = "Samples") 


######## Group-wise ############

#Extract samples belong to each group with name 
Non_ML1<- as.data.frame(log_df[grep('^Non.malignant', names(log_df))]) 
Claudin1 <- as.data.frame(log_df[grep('^Claudin', names(log_df))])

#Compute the mean of samples for each gene within each group 
Non_ML<- rowMeans(Non_ML1) 
Claudin <- rowMeans(Claudin1)

#Bind groups together column-wise 
group <- cbind(Non_ML, Claudin) 

#Provide the column names to both groups of samples 
colnames(group) <- c("Non-malignant", "Claudin-low")

#Create a long formatted dataframe 
df_group <- melt(group) 

#print length of group object 
print(length(df_group))

#Draw boxplot  
pp <- ggplot(df_group, aes(factor(Var2), value)) + geom_boxplot(aes(fill = Var2)) 

#Add axis title, legend title and adjust font size 
pp + theme(legend.key.size = unit(0.9, "cm"), legend.text = element_text(color= "Black", size= 9), axis.text.x = element_text(color= "Black", size = 9, angle = 90), axis.text.y = element_text(size=10, angle=45)) + xlab("Groups") + ylab("Gene Expression (Log[FPKM])") + labs(fill = "Group")
