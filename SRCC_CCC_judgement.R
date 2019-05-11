## Spearman’s rank correlation coefficient(SRCC) is a measure of monotone (not necessarily linear)
# association between two variables. The value of SRCC lies in between −1 and +1, with values 
# close to +1 indicating that the two sets of values are positively associated to each other, 
# values close to −1 indicating that the two sets of values are negatively associated to each other, 
# and values close to 0 indicating that the two sets of values are not associated with each other. 
# Thus, if SRCC is high (i.e., close to 1), it is likely that a normalization would be able to bring
# the two sets of values into agreement, whereas, if SRCC is low (i.e., much lower than 1), 
# it is unlikely that a normalization would be able to bring the two sets of values into agreement.
# The value of Spearman’s rank correlation coefficient is unchanged by a monotone normalization 
# procedure. Therefore, while it is a good measure of whether a normalization would be successful, 
# it cannot be used to judge the success of a monotone normalization procedure.

# The correlation coefficients can be plotted using a heat map representation. 
# ggplot2 provides the geom_tile geometric object for this purpose. 
# In order to plot the correlation matrix we need to meld the data frame first(reshape2 library).
library(ggplot2)
library(reshape2)
library(Hmisc)

d <- NULL 
d <- GSE18044_log2
cormatrix_GSE18044_SRCC <- rcorr(as.matrix(d), type='spearman')
cordata_GSE18044_SRCC <-  melt(cormatrix_GSE18044_SRCC$r)

ggplot(cordata_GSE18044_ps, aes(x=Var1, y=Var2, fill=value)) + 
  scale_fill_gradientn(limits = c(-1,1), 
                       colours=c("red","black", "white")) +
  geom_tile() + xlab("") + ylab("") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## Instead, once a normalization has been performed, the degree of success of the normalization 
# can be assessed via the concordance correlation coefficient(CCC), an index that quantifies 
# the degree of agreement between two sets of numbers.

# A function for creating a matrix for concordance correlation coefficient:

concordance_cc <- function(df){
  vec1 <- NULL
  vec2 <- NULL
  vec3 <- NULL
  matrix1 <- NULL
  for (i in 1:ncol(df)){
    for (j in 1:ncol(df)){
      vec3 <- c(vec3, (2*cov(x = df[,i], y = df[,j])/
                         (var(df[,i])+var(df[,j])+
                            (mean(df[,i])-mean(df[,j]))^2)))
      vec2 <- c(vec2, colnames(df)[i])
      vec1 <- c(vec1, colnames(df)[j])
    }
  }
  matrix1 <- data.frame("Var1" = vec1, "Var2" = vec2, "concordance" = vec3)
  return(matrix1)
}

# Applying the function for CCC:
df <- NULL
df <- GSE18044_QN
ccc_GSE18044_QN <- concordance_cc(df)

# Ploting a resulted matrix by heat map plot with tiles
ggplot(ccc_GSE18044_QN, aes(x=Var1, y=Var2, fill=concordance)) + 
  scale_fill_gradientn(limits = c(0,1), 
                       colours=c("black", "white")) +
  geom_tile() + xlab("") + ylab("") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))