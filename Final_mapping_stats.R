#Load libraries 
library(ggplot2) 
library(reshape2)


#Barplot for alignment Rate for single type 


input <- read.table("Alignment_Rate_input.txt", sep="\t", header = TRUE)

head(input)
bb <- ggplot(input, aes(x=Sample, y=Overall_Alignment_Rate)) + geom_bar(stat = "identity", width=0.5) 

 plot6 <- bb + theme(legend.key.size = unit(0.6, "cm"), legend.text = element_text(color= "Black", size= 6), axis.text.x = element_text(color= "Black", size= 6, angle = 90), axis.text.y = element_text(size=7, angle=45)) + xlab("Samples") + ylab("Alignment Rate (%)") + labs(fill = "Group") + 
   labs(title= paste("Alignment Rate for Samples")) +  theme(plot.title = element_text(hjust = 0.5, size=10))+
   scale_y_continuous(labels = scales::comma)
plot6

#barplot(as.matrix(t(input)), las=2, cex.axis = 0.5, cex.lab=0.2)

grid <- grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow=3, ncol=2)





################# Barplot for alignment Rate for multiple columns #########


input <- read.table("Overall_alignment.txt", sep="\t",  check.names = F, header = TRUE)

#head(input)

melt_mat <- melt(input)
head(melt_mat)


bb1 <- ggplot(melt_mat, aes(x=Sample, y=value)) + geom_bar(aes(fill = variable),stat = "identity",position = "dodge")
  
  #geom_bar(stat = "identity", fill=variable, width=0.5) 

plot7 <- bb1 + theme(legend.key.size = unit(0.6, "cm"), legend.text = element_text(color= "Black", size= 6), axis.text.x = element_text(color= "Black", size= 6, angle = 90), axis.text.y = element_text(size=7, angle=45)) + xlab("Samples") + ylab("Overall Alignment Rate (%)") + labs(fill = "Group") + labs(title= paste("Overall Alignment Rate for Samples")) +  theme(plot.title = element_text(hjust = 0.5, size=10))
plot7



