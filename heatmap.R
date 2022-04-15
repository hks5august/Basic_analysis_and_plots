
data <- read.table("histone_input.txt", header=TRUE, sep = "\t", row.names=1, check.names = F)
dim(data)
mat <- as.matrix(data)
heatmap(mat, cexRow = 0.32, cexCol = 0.45, Rowv = NA, Colv = NA)
?heatmap
