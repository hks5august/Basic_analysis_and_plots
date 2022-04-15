#Load libraries 
library(ggplot2) 
library(reshape2)
library(ggpubr)
library(gridExtra)


#install.packages("rasterdiv")
library(rasterdiv)

data3 <- read.table("Shannon_index_input.txt", sep="\t", header=TRUE) ## for groups
data3 <- read.table("Shannon_index_sample_wise_input.txt", sep="\t", header=TRUE) ## for samples

head(data3)

data3_t <- as.data.frame(t(data3))
#install.packages("vegan")
library(vegan)

#exp(diversity(data3_t, index="shannon"))
div <- diversity(data3_t, index="shannon")
div

div1 <- as.data.frame(div)
dim(div1)
div1
colnames(div1) <- c( "Control", "M1", "M10" , "M100")

Write.table(div1, shannon_result.txt, sep="\t")

###barplot for shannon index ##############

input_b <- read.table("Shannon_barplot_input.txt",  header=TRUE, sep="\t") #### Groups
input_b1 <- read.table("Shannon_barplot_input_samples.txt",  header=TRUE, sep="\t") ### Sample wise


## Barplot groupwise
b_sh <- ggplot(input_b, aes(x=Group, y=Shannon_index, fill=Group)) + geom_col() + theme(axis.text.x=element_text(angle=45, hjust=1))
#Add axis title, legend title and adjust font size 
BSH_plot1 <- b_sh + theme(legend.key.size = unit(0.7, "cm"), legend.text = element_text(color= "Black", size= 7), axis.text.x = element_text(color= "Black", size= 10, angle = 90), axis.text.y = element_text(size=10, angle=45)) + xlab("Groups") + ylab("Shannon Index") + labs(fill = "Group") + labs(title= paste(" ", "Barplot for Shannon Index")) +  theme(plot.title = element_text(hjust = 0.5))
BSH_plot1


## Barplot Sample-wise
head(input_b1)
b_sh2 <- ggplot(input_b1, aes(x=Sample, y=Shannon_Index, fill=Sample)) + geom_col() + theme(axis.text.x=element_text(angle=45, hjust=1))
BSH_plot2 <- b_sh2 + theme(legend.key.size = unit(0.7, "cm"), legend.text = element_text(color= "Black", size= 7), axis.text.x = element_text(color= "Black", size= 10, angle = 90), axis.text.y = element_text(size=10, angle=45)) + xlab("Samples") + ylab("Shannon Index") + labs(fill = "Sample") + labs(title= paste(" ", "Barplot for Shannon Index")) +  theme(plot.title = element_text(hjust = 0.5))
BSH_plot2


###Boxplot Shannon index ####

data_shann_box <- read.table("Shannon_index_boxplot_input.txt", header=TRUE, sep="\t", check.names=FALSE) ### With M100 group
data_shann_box <- read.table("Shannon_index_boxplot_input_ctrl_vs_treatment.txt", header=TRUE, sep="\t", check.names=FALSE) ### Without M100 group, ctrl vs treatment



head(data_shann_box)


p_sh <- ggboxplot(data_shann_box, x = "Group", y = "Shannon_Index",  color = "Group")
p_sh

p_sh1 <- p_sh + stat_compare_means()

p_sh + theme(legend.key.size = unit(1.1, "cm"), legend.text = element_text(color= "Black", size= 10), axis.text.x = element_text(color= "Black", size= 10, angle=90), axis.text.y = element_text(size=10, angle=45)) + xlab("Group") + ylab("Shannon Index") +  labs(title= paste("", "Boxplot for Shannon with M100")) +  theme(plot.title = element_text(hjust = 0.5))

### boxplot shannon index without M100

data_shann_box2 <- read.table("Shannon_index_boxplot_input_without_m100.txt", header=TRUE, sep="\t", check.names=FALSE) ### Without M100  -sample wise
data_shann_box2 <- read.table("Shannon_index_boxplot_input_without_m100_ctrl_treatment.txt", header=TRUE, sep="\t", check.names=FALSE) ### Without M100 group, ctrl vs treatment


head(data_shann_box2)


p_sh2 <- ggboxplot(data_shann_box2, x = "Group", y = "Shannon_Index",  color = "Group")
p_sh2

p_sh21 <- p_sh2 + stat_compare_means()

p_sh2 + theme(legend.key.size = unit(1.1, "cm"), legend.text = element_text(color= "Black", size= 10), axis.text.x = element_text(color= "Black", size= 10, angle=90), axis.text.y = element_text(size=10, angle=45)) + xlab("Group") + ylab("Shannon Index") +  labs(title= paste("", "Boxplot for Shannon without M100")) +  theme(plot.title = element_text(hjust = 0.5))





#T-test between groups #####
#Boxplot for IC50 with M100 ####

library(ggpubr)
data_ic50 <- read.table("Control_vs_treatment_IC50_boxplot_input.txt", header=TRUE, sep="\t", check.names=FALSE)


head(data_ic50)

#ggboxplot(ToothGrowth, x = "supp", y = "len",color = "supp", palette = "jco",add = "jitter")

#p <- ggboxplot(data, x = "Sample", y = "ic50",  color = "Sample", palette = "RdGn", add = "jitter")

#p
p_i <- ggboxplot(data_ic50, x = "Sample", y = "ic50",  color = "Sample")

p_ic <- p_i + stat_compare_means()

p_ic + theme(legend.key.size = unit(1.1, "cm"), legend.text = element_text(color= "Black", size= 10), axis.text.x = element_text(color= "Black", size= 10, angle=90), axis.text.y = element_text(size=10, angle=45)) + xlab("Group") + ylab("IC50") +  labs(title= paste("", "Boxplot for IC50 with M100")) +  theme(plot.title = element_text(hjust = 0.5))

  
## Boxplot IC50 without M100 ###
data_ic50_2 <- read.table("Control_vs_treatment_IC50_boxplot_input2.txt", header=TRUE, sep="\t", check.names=FALSE)

head(data_ic50_2)

p_i_2 <- ggboxplot(data_ic50_2, x = "Sample", y = "ic50",  color = "Sample")

p_ic_2 <- p_i_2 + stat_compare_means()

p_ic_2 + theme(legend.key.size = unit(1.1, "cm"), legend.text = element_text(color= "Black", size= 10), axis.text.x = element_text(color= "Black", size= 10, angle=90), axis.text.y = element_text(size=10, angle=45)) + xlab("Group") + ylab("IC50") +  labs(title= paste("", "Boxplot for IC50 without M100")) +  theme(plot.title = element_text(hjust = 0.5))




#Boxplot for Percentile Rank with M100 ###
data1_pr <- read.table("Control_vs_treatment_Percentile_Rank_boxplot_input.txt", header=TRUE, sep="\t", check.names=FALSE)


head(data1_pr)


p1_pr <- ggboxplot(data1_pr, x = "Sample", y = "percentile_rank",  color = "Sample")

ps1_pr <- p1_pr + stat_compare_means()
#p + stat_compare_means(method = "t.test")
ps1_pr + theme(legend.key.size = unit(1.1, "cm"), legend.text = element_text(color= "Black", size= 10), axis.text.x = element_text(color= "Black", size= 10, angle=90), axis.text.y = element_text(size=10, angle=45)) + xlab("Group") + ylab("Percentile Rank") + labs(title= paste("", "Boxplot for Percentile Rank with M100")) +  theme(plot.title = element_text(hjust = 0.5))


#Boxplot for Percentile Rank without M100 ###
data1_pr1 <- read.table("Control_vs_treatment_Percentile_Rank_boxplot_input2.txt", header=TRUE, sep="\t", check.names=FALSE)


head(data1_pr1)


p1_pr1 <- ggboxplot(data1_pr1, x = "Sample", y = "percentile_rank",  color = "Sample")

ps1_pr1 <- p1_pr1 + stat_compare_means()
#p + stat_compare_means(method = "t.test")
ps1_pr1 + theme(legend.key.size = unit(1.1, "cm"), legend.text = element_text(color= "Black", size= 10), axis.text.x = element_text(color= "Black", size= 10, angle=90), axis.text.y = element_text(size=10, angle=45)) + xlab("Group") + ylab("Percentile Rank") + labs(title= paste("", "Boxplot for Percentile Rank without M100")) +  theme(plot.title = element_text(hjust = 0.5))



########
##Boxplot for Clone fraction ####
data_cf1 <- read.table("Control_vs_treatment_Clone_fraction_with_M100_boxplot_input.txt", header=TRUE, sep="\t", check.names=FALSE)


head(data_cf1)


#p
pF <- ggboxplot(data_cf1, x = "Group", y = "Clone_Fraction",  color = "Group")

psF <- pF + stat_compare_means()
#p + stat_compare_means(method = "t.test")
psF + theme(legend.key.size = unit(1.1, "cm"), legend.text = element_text(color= "Black", size= 10), axis.text.x = element_text(color= "Black", size= 10, angle=90), axis.text.y = element_text(size=10, angle=45)) + xlab("Group") + ylab("Clone Fraction") +  labs(title= paste("", "Boxplot for Clone Fraction with M100")) +  theme(plot.title = element_text(hjust = 0.5))




##Boxplot for Clone count with M100 ####
data_cc1 <- read.table("Control_vs_treatment_Clone_count_with_M100_boxplot_input.txt", header=TRUE, sep="\t", check.names=FALSE)


head(data_cc1)


#p
pc1 <- ggboxplot(data_cc1, x = "Group", y = "Clone_Count",  color = "Group")

pcs1 <- pc1 + stat_compare_means()
#p + stat_compare_means(method = "t.test")
pcs1 + theme(legend.key.size = unit(1.1, "cm"), legend.text = element_text(color= "Black", size= 10), axis.text.x = element_text(color= "Black", size= 10, angle=90), axis.text.y = element_text(size=10, angle=45)) + xlab("Group") + ylab("Clone Count") +  labs(title= paste("", "Boxplot for Clone Count with M100")) +  theme(plot.title = element_text(hjust = 0.5))



######### after M100 Removal #############

##Boxplot for Clone fraction ####
data_cf <- read.table("Control_vs_treatment_Clone_fraction_after_M100rem_boxplot_input.txt", header=TRUE, sep="\t", check.names=FALSE)


head(data_cf)


#p
p <- ggboxplot(data_cf, x = "Group", y = "Clone_Fraction",  color = "Group")

ps <- p + stat_compare_means()
#p + stat_compare_means(method = "t.test")
ps + theme(legend.key.size = unit(1, "cm"), legend.text = element_text(color= "Black", size= 6), axis.text.x = element_text(color= "Black", size= 10, angle=90), axis.text.y = element_text(size=10, angle=45)) + xlab("Group") + ylab("Clone Fraction") + labs(title= paste("", "Boxplot for Clone Fraction without M100")) +  theme(plot.title = element_text(hjust = 0.5))




##Boxplot for Clone count afte m100 removal ####
data_cc <- read.table("Control_vs_treatment_Clone_count_after_M100rem_boxplot_input.txt", header=TRUE, sep="\t", check.names=FALSE)


head(data_cc)


#p
pc <- ggboxplot(data_cc, x = "Group", y = "Clone_Count",  color = "Group")

pcs <- pc + stat_compare_means()
#p + stat_compare_means(method = "t.test")
pcs + theme(legend.key.size = unit(1.1, "cm"), legend.text = element_text(color= "Black", size= 10), axis.text.x = element_text(color= "Black", size= 10, angle=90), axis.text.y = element_text(size=10, angle=45)) + xlab("Group") + ylab("Clone Count") +  labs(title= paste("", "Boxplot for Clone Count without M100")) +  theme(plot.title = element_text(hjust = 0.5))



################# Barplots #########

data <- read.table("Boxplot_IC50_input.txt", header=TRUE, sep="\t")
data1 <- read.table("Boxplot_percentile_rank_input.txt", header=TRUE, sep="\t")


df <- melt(data)
df1 <- melt(data1)

head(df)


#Draw boxplot  
pp <- ggplot(df, aes(factor(Sample), value)) + geom_boxplot(aes(fill = Sample)) 

#Add axis title, legend title and adjust font size 
plot <- pp + theme(legend.key.size = unit(0.7, "cm"), legend.text = element_text(color= "Black", size= 7), axis.text.x = element_text(color= "Black", size= 10, angle = 90), axis.text.y = element_text(size=10, angle=45)) + xlab("Groups") + ylab("IC50") + labs(fill = "Group") + labs(title= paste(" ", "Boxplot for IC50")) +  theme(plot.title = element_text(hjust = 0.5))
plot


#Draw boxplot  
pp1 <- ggplot(df1, aes(factor(Sample), value)) + geom_boxplot(aes(fill = Sample)) 

plot1 <- pp1 + theme(legend.key.size = unit(0.7, "cm"), legend.text = element_text(color= "Black", size= 7), axis.text.x = element_text(color= "Black", size= 10, angle = 90), axis.text.y = element_text(size=10, angle=45)) + xlab("Groups") + ylab("Percentile Rank") + labs(fill = "Group") + labs(title= paste(" ", "Boxplot for Percentile Rank")) +  theme(plot.title = element_text(hjust = 0.5))

plot1



data2 <- read.table("Barplot_input_Sample_with_fraction_clones.txt", header=TRUE, sep="\t")

data2 <- read.table("Barplot_input_Sample_with_fraction_clones2.txt", header=TRUE, sep="\t")


head(data2)

bb <- ggplot(data2, aes(x=Sample, y=cloneFraction)) + geom_bar(stat = "identity", width=0.5) 

plot3 <- bb + theme(legend.key.size = unit(0.6, "cm"), legend.text = element_text(color= "Black", size= 6), axis.text.x = element_text(color= "Black", size= 10, angle = 90), axis.text.y = element_text(size=10, angle=45)) + xlab("Groups") + ylab("Clone Fraction") + labs(fill = "Group") + 
  labs(title= paste("CloneFraction in Group")) +  theme(plot.title = element_text(hjust = 0.5, size=10))+
  scale_y_continuous(labels = scales::comma)
plot3


