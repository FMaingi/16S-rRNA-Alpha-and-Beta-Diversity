
library(readxl)
Feature_table <- read_excel("PQ.xlsx")
View (Feature_table)
library(vegan)
library(plyr)
library(Rarefy)
#RICHNESS
ddply(PQ,~sample,function(x) {data.frame(RICHNESS=sum(x[-1]>0))})

#ABUNDANCE
ddply(PQ,~sample,function(x) {data.frame(ABUNDANCE=sum(x[-1]))})

#RAREFACTION
rarefy(alphadiversity1[-1], sample=5, MARGIN=1)
#or 
ddply(alphadiversity1,~sample,function(x) {data.frame(RAREFY=rarefy(x[-1], sample=5, MARGIN=1))})

#SHANNON DIVERISTY
diversity(alphadiversity1[-1], index="shannon")
#OR
ddply(PQ,~sample,function(x) {data.frame(SHANNON=diversity(x[-1], index="shannon"))})


#EVENNESS
S <- apply(Feature_table[,-1]>0,1,sum)
diversity(Feature_table[-1], index="simpson")/log(S)
#OR
ddply(PQ,~sample,function(x) {data.frame(shannon=diversity(x[-1], index="shannon")/log(sum(x[-1]>0)))})


#EFFECTIVE (TRUE) DIVERSITY
exp(diversity(Feature_table[-1], index="shannon"))
#OR
ddply(PQ,~sample,function(x) {data.frame(TRUE_SHANNON=exp(diversity(x[-1], index="shannon")))})




#BETA DIVERSTIY 
beta_diversity_feat_table<- as.matrix (betaPQ)


vegdist(beta_diversity_feat_table, method="bray", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = FALSE) 
vegdist(beta_diversity_feat_table, method="jaccard", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = FALSE) 
vegdist(beta_diversity_feat_table, method="euclidean", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = FALSE) 


anosim(beta_div_feature_table, grouping= beta_div_feature_table$Sites, permutations = 1000, distance = "bray", strata = NULL, parallel = 1)
anosim(beta_diversity_feat_table, grouping= beta_diversity_feat_table$Sites, permutations = 1000, distance = "bray", strata = NULL, parallel = 1)



bet_div <- betadivmatrix[, -1]

fit<-cmdscale(bet_div, eig=TRUE, k=2)
x <- fit$points[, 1]
y <- fit$points[, 2]
plot(x,y)
fit

JaccardTE <- JaccardTE[, -1]

fit<-cmdscale(JaccardTE, eig=TRUE, k=2)
x <- fit$points[, 1]
y <- fit$points[, 2]
plot(x,y)
fit
