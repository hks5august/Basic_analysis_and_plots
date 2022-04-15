#Load libraries 
library(ggplot2) 
library(reshape2)
library(ggpubr)
library(gridExtra)

#import Data 
df <- read.table("alignment_stats.txt", header=TRUE,  sep="\t", row.names= 1)


#Group wise Alignment wisw

df1 <- df[1,]
title <- "Alignment Not Unique"
df1_t <- as.data.frame(t(df1))
df1_t
#Extract samples belong to each group with column name 
#dT<- as.data.frame(df1_t[grep('^dT', names(df1_t))]) 
#rRNA <- as.data.frame(df1[grep('^rRNA', names(df1))])
#Total<- as.data.frame(df1[grep('^Total', names(df1))]) 

#Extract samples belong to each group with row name 
Control<-as.data.frame(df1_t[grep("Control", rownames(df1_t)), ])
D10<-as.data.frame(df1_t[grep("D10", rownames(df1_t)), ])
D25<-as.data.frame(df1_t[grep("D25", rownames(df1_t)), ])
D50<-as.data.frame(df1_t[grep("D50", rownames(df1_t)), ])

#Bind groups together column-wise 
group <- cbind(Control, D10, D25, D50) 

#Provide the column names to both groups of samples 
colnames(group) <- c("Control", "D10", "D25", "D50")

group
#Create a long formatted dataframe 
df_group <- melt(group) 

#print length of group object 
head(df_group)

#Draw boxplot  
pp <- ggplot(df_group, aes(factor(variable), value)) + geom_boxplot(aes(fill = variable)) 

#Add axis title, legend title and adjust font size 
plot1 <- pp + theme(legend.key.size = unit(0.6, "cm"), legend.text = element_text(color= "Black", size= 6), axis.text.x = element_text(color= "Black", size= 6, angle = 90), axis.text.y = element_text(size=7, angle=45)) + xlab("") + ylab("") + 
  labs(fill = "Group") + labs(title= paste("", title))+  
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  scale_y_continuous(labels = scales::comma)

plot1



df2 <- df[2,]
title <- "Ambigous"
df2_t <- as.data.frame(t(df2))
df2_t
#Extract samples belong to each group with column name 
#dT<- as.data.frame(df1_t[grep('^dT', names(df1_t))]) 
#rRNA <- as.data.frame(df1[grep('^rRNA', names(df1))])
#Total<- as.data.frame(df1[grep('^Total', names(df1))]) 

#Extract samples belong to each group with row name 
Control<-as.data.frame(df2_t[grep("Control", rownames(df2_t)), ])
D10<-as.data.frame(df2_t[grep("D10", rownames(df2_t)), ])
D25<-as.data.frame(df2_t[grep("D25", rownames(df2_t)), ])
D50<-as.data.frame(df2_t[grep("D50", rownames(df2_t)), ])

#Bind groups together column-wise 
group2 <- cbind(Control, D10, D25, D50) 

#Provide the column names to both groups of samples 
colnames(group2) <- c("Control", "D10", "D25", "D50")

#Create a long formatted dataframe 
df_group2 <- melt(group2) 

#print length of group object 
head(df_group2)

#Draw boxplot  
pp2 <- ggplot(df_group2, aes(factor(variable), value)) + geom_boxplot(aes(fill = variable)) 

#Add axis title, legend title and adjust font size 
plot2 <- pp2 + theme(legend.key.size = unit(0.6, "cm"), legend.text = element_text(color= "Black", size= 6), axis.text.x = element_text(color= "Black", size= 6, angle = 90), axis.text.y = element_text(size=7, angle=45)) + xlab("") + ylab("") + labs(fill = "Group") + 
  labs(title= paste(" ", title)) +  
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  scale_y_continuous(labels = scales::comma)

plot2



df3 <- df[3,]
title <- "No Feature"
df3_t <- as.data.frame(t(df3))
df3_t
#Extract samples belong to each group with column name 
#dT<- as.data.frame(df1_t[grep('^dT', names(df1_t))]) 
#rRNA <- as.data.frame(df1[grep('^rRNA', names(df1))])
#Total<- as.data.frame(df1[grep('^Total', names(df1))]) 

#Extract samples belong to each group with row name 
Control<-as.data.frame(df3_t[grep("Control", rownames(df3_t)), ])
D10<-as.data.frame(df3_t[grep("D10", rownames(df3_t)), ])
D25<-as.data.frame(df3_t[grep("D25", rownames(df3_t)), ])
D50<-as.data.frame(df3_t[grep("D50", rownames(df3_t)), ])

#Bind groups together column-wise 
group3 <- cbind(Control, D10, D25, D50) 

#Provide the column names to both groups of samples 
colnames(group3) <- c("Control", "D10", "D25", "D50")

group
#Create a long formatted dataframe 
df_group3 <- melt(group3) 

#print length of group object 
head(df_group3)

#Draw boxplot  
pp3 <- ggplot(df_group3, aes(factor(variable), value)) + geom_boxplot(aes(fill = variable)) 

#Add axis title, legend title and adjust font size 
plot3 <- pp3 + theme(legend.key.size = unit(0.6, "cm"), legend.text = element_text(color= "Black", size= 6), axis.text.x = element_text(color= "Black", size= 6, angle = 90), axis.text.y = element_text(size=7, angle=45)) + xlab("") + ylab("") + labs(fill = "Group") + labs(title= paste(" ", title)) + 
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  scale_y_continuous(labels = scales::comma)

plot3


df4 <- df[2,]
title <- "Not Aligned"
df4_t <- as.data.frame(t(df4))
df4_t
#Extract samples belong to each group with column name 
#dT<- as.data.frame(df1_t[grep('^dT', names(df1_t))]) 
#rRNA <- as.data.frame(df1[grep('^rRNA', names(df1))])
#Total<- as.data.frame(df1[grep('^Total', names(df1))]) 

#Extract samples belong to each group with row name 
Control<-as.data.frame(df4_t[grep("Control", rownames(df4_t)), ])
D10<-as.data.frame(df4_t[grep("D10", rownames(df4_t)), ])
D25<-as.data.frame(df4_t[grep("D25", rownames(df4_t)), ])
D50<-as.data.frame(df4_t[grep("D50", rownames(df4_t)), ])

#Bind groups together column-wise 
group4 <- cbind(Control, D10, D25, D50) 

#Provide the column names to both groups of samples 
colnames(group4) <- c("Control", "D10", "D25", "D50")
#Create a long formatted dataframe 
df_group4 <- melt(group4) 

#print length of group object 
head(df_group4)

#Draw boxplot  
pp4 <- ggplot(df_group4, aes(factor(variable), value)) + geom_boxplot(aes(fill = variable)) 

#Add axis title, legend title and adjust font size 
plot4 <- pp4 + theme(legend.key.size = unit(0.6, "cm"), legend.text = element_text(color= "Black", size= 6), axis.text.x = element_text(color= "Black", size= 6, angle = 90), axis.text.y = element_text(size=7, angle=45)) + xlab("") + ylab("") + labs(fill = "Group") + labs(title= paste(" ", title)) +  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  scale_y_continuous(labels = scales::comma)

plot4


df5 <- df[5,]
title <- "Too Low Quality"
df5_t <- as.data.frame(t(df5))
df5_t
#Extract samples belong to each group with column name 
#dT<- as.data.frame(df1_t[grep('^dT', names(df1_t))]) 
#rRNA <- as.data.frame(df1[grep('^rRNA', names(df1))])
#Total<- as.data.frame(df1[grep('^Total', names(df1))]) 

#Extract samples belong to each group with row name 
Control<-as.data.frame(df5_t[grep("Control", rownames(df5_t)), ])
D10<-as.data.frame(df5_t[grep("D10", rownames(df5_t)), ])
D25<-as.data.frame(df5_t[grep("D25", rownames(df5_t)), ])
D50<-as.data.frame(df5_t[grep("D50", rownames(df5_t)), ])

#Bind groups together column-wise 
group5 <- cbind(Control, D10, D25, D50) 

#Provide the column names to both groups of samples 
colnames(group5) <- c("Control", "D10", "D25", "D50")


#Create a long formatted dataframe 
df_group5 <- melt(group5) 

#print length of group object 
head(df_group5)

#Draw boxplot  
pp5 <- ggplot(df_group5, aes(factor(variable), value)) + geom_boxplot(aes(fill = variable)) 

#Add axis title, legend title and adjust font size 
plot5 <- pp5 + theme(legend.key.size = unit(0.6, "cm"), legend.text = element_text(color= "Black", size= 6), axis.text.x = element_text(color= "Black", size= 6, angle = 90), axis.text.y = element_text(size=7, angle=45)) + xlab("") + ylab("") + labs(fill = "Group") + labs(title= paste(" ", title)) +  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  scale_y_continuous(labels = scales::comma)

plot5


#Barplot for alignment Rate


input <- read.table("Alignment_Rate_input.txt", sep="\t", header = TRUE)
input <- read.table("Overall_alignment_dT_total.txt", sep="\t", header = TRUE)

head(input)
bb <- ggplot(input, aes(x=Sample, y=Overall_Alignment_Rate)) + geom_bar(stat = "identity", width=0.5) 

 plot6 <- bb + theme(legend.key.size = unit(0.6, "cm"), legend.text = element_text(color= "Black", size= 6), axis.text.x = element_text(color= "Black", size= 6, angle = 90), axis.text.y = element_text(size=7, angle=45)) + xlab("Samples") + ylab("Alignment Rate (%)") + labs(fill = "Group") + 
   labs(title= paste("Alignment Rate for Samples")) +  theme(plot.title = element_text(hjust = 0.5, size=10))+
   scale_y_continuous(labels = scales::comma)
plot6

#barplot(as.matrix(t(input)), las=2, cex.axis = 0.5, cex.lab=0.2)

grid <- grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow=3, ncol=2)








################# Barplot for alignment Rate for multiple columns #########


#input <- read.table("Overall_alignment.txt", sep="\t",  check.names = F, header = TRUE)

input <- read.table("Overall_alignment_dT_total.txt", sep="\t",  check.names = F, header = TRUE)


#head(input)

melt_mat <- melt(input)
head(melt_mat)


bb1 <- ggplot(melt_mat, aes(x=Sample, y=value)) + geom_bar(aes(fill = variable),stat = "identity",position = "dodge")
  
  #geom_bar(stat = "identity", fill=variable, width=0.5) 

plot7 <- bb1 + theme(legend.key.size = unit(0.6, "cm"), legend.text = element_text(color= "Black", size= 6), axis.text.x = element_text(color= "Black", size= 6, angle = 90), axis.text.y = element_text(size=7, angle=45)) + xlab("Samples") + ylab("Overall Alignment Rate (%)") + labs(fill = "Group") + labs(title= paste("Overall Alignment Rate for Samples")) +  theme(plot.title = element_text(hjust = 0.5, size=10))
plot7



