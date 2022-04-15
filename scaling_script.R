library("caret")

data <- read.table("QN_input.txt", sep="\t", row.names=1, header=1)
# remove near zero variation for the columns at least or 80% of the values are the same
# this function creates the filter 
#nzv <- preProcess(train_exp,method="nzv",uniqueCut = 20)

# apply the above created filter using "predict" function
#nzv_exp_tr <- predict(nzv,train_exp)
#nzv_exp_te <- predict(nzv,test_exp)

# center & scaling
processCenter <- preProcess(data, method = c("center", "scale"))

# apply center & scaling on traning and test data
#Norm_tr_exp <- predict(processCenter,nzv_exp_tr)
#Norm_te_exp <- predict(processCenter,nzv_exp_te)

scaled_data <- predict(processCenter,data)

#round up value upto 3 digits
#Norm_tr_exp <- round(Norm_tr_exp,3)
#Norm_te_exp <- round(Norm_te_exp,3)

scaled_data  <- round(scaled_data,2)
#Write results into a file
write.table(scaled_data ,file="scaled_data.txt", sep='\t',  quote = F,row.names = TRUE)
