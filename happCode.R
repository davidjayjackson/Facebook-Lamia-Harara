library(readr)
library(tidyverse)
summary(happiness)
#REMOVED COLUMNs 4,5,6
colnames(happiness)
ColumnSelected = c(1:3,7:12) 
Happy.df = happiness[,ColumnSelected]
# ggpairs for Happy.df without the first column; 
# the second column has still many categories, so the graph is jammed but it works
library(GGally)
ggpairs(Happy.df[,2:9], mapping = ggplot2::aes(colour=Happy.df$`Regional indicator`), 
        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1)))
par(mfrow=c(1,2))
hist(Happy.df$`Social support`)
# Define dependent variable and independent variables
DV = Happy.df[,3]
IV = Happy.df[,4:9]
IV5 =Happy.df[,5]
# conduct factor analysis using 1) princomp and 2) factanal in psych library
# Some examples of these two and related functions are given below
fit <- princomp(IV, cor=TRUE)
summary(fit)
loadings(fit) 
plot(fit,type="lines") 
fit$scores 
library(ggfortify)
autoplot(fit)
# For example, plot factor 1 by factor 2
Score <- fit$scores[,1:2]
plot(Score ,type="n") 
text(Score ,labels=names(IV),cex=.7) 

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors,
# with varimax rotation
library(psych)
Score <- factanal(IV, 3, rotation="varimax")
print(Score, digits=2, cutoff=.3, sort=TRUE)


# A vector countaining  3 regions
RegionSelected = c("East Asia","Middle East and North Africa","Sub-Saharan Africa")
ImpoReg <- Happy.df %>% filter(`Regional indicator` %in% RegionSelected)
ggpairs(ImpoReg[,2:9], mapping = ggplot2::aes(colour=ImpoReg$`Regional indicator`), 
        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1)))

# A vector countaining  1 region
Region = c("Middle East and North Africa")
MENA <- Happy.df %>% filter(`Regional indicator` %in% Region)
ggpairs(MENA[,2:9], mapping = ggplot2::aes(colour=MENA$`Regional indicator`), 
        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1)))



#linear regression for 
RegSo<-lm(DV~IV)
summary(RegSo) 
#Polynomial relationship between Ladderscore and Social support
IV5square <- IV5^2
RegSo<-lm(DV~IV5+IV5square)
summary(RegSo) #Error again









