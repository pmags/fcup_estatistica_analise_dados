
# ======================================================================================= #
# Script Name : pca_analysis                                                                                           
# Purpose     : pca analysis script                                                                     
# Args        : 
# Date        : Wed Apr 13 12:00:43 2022   
# Author      : Pedro Magalhães                                                
# Email       : pedro.magalhaes@mosaic.pt                                           
# ======================================================================================= #


# libraries ---------------------------------------------------------------

library(tidyverse)
library(FactoMineR)


PCA_Bank <-PCA(Bank[,-c(12,13)])
PCA_Bank$svd$V # Eigenvectors
PCA_Bank$eig # Eigenvalues
## plot of the eigenvalues:
barplot(PCA_Bank$eig[,1],main="Eigenvalues",names.arg=1:nrow
        (PCA_Bank$eig))