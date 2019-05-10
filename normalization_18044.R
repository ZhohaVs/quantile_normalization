str(GSE18044_non.norm)
# Dataset has been created either with numeric and character classes variables
# Command below creates a list with character class varibles
srt_list <- colnames(GSE18044_non.norm[,sapply(GSE18044_non.norm, mode) == "character"])
#This iterration changes chr class to num class within dataset
for (i in srt_list){
  GSE18044_non.norm[,i] <- as.numeric(GSE18044_non.norm[,i])
} 
str(GSE18044_non.norm)
any(is.na(GSE18044_non.norm))
colSums(is.na(GSE18044_non.norm))
# As a result, NA values has been created. One has to remove rows with this values.
GSE18044_non.norm_rm_na <- GSE18044_non.norm[complete.cases(GSE18044_non.norm), ]
# Do log2 transformation 
GSE18044_log2 <- log2(GSE18044_non.norm_rm_na)


# A function of quantile normalisation:
quantile_normalisation <- function(df){
  df_rank <- apply(df,2,rank,ties.method="min")
  df_sorted <- data.frame(apply(df, 2, sort))
  df_median <- apply(df_sorted, 1, median)
  
  index_to_median <- function(my_index, my_median){
    return(my_median[my_index])
  }
  
  df_final <- data.frame(apply(df_rank, 2, index_to_median, my_median=df_median))
  rownames(df_final) <- rownames(df)
  return(df_final)
}


# Apply the function to log2_dataset
df <-  NULL
df <- GSE18044_log2
GSE18044_QN <- quantile_normalisation(df)

write.csv(GSE18044_QN, 'GSE18044_QN')
