#*******************************************************************************
#********************************************************************************
# 
## R scipts "Evenness" for Chao and Ricotta (2019) Ecology paper. 
## This R code is for computing Figures 2, 3 and 4 of Chao and Ricotta (2019) paper.
# NOTE: The packages "ggplot2", "dplyr", "ade4", "reshape2", "ggpubr", "phytools", "ape" must be 
# installed and loaded before running the scripts. 
# 
#
# The following R scripts include two parts:  
# (1). Script for computing the profiles for six classes of evenness measures (see Figure 2 in Chao and Ricotta's paper).
# (2). Script for computing the contribution of each species/node to taxonomic dissimarity and/or phylogenetic
#      dissimarity measures (see Figures 3 and 4 in Chao and Ricotta's paper)
# 
#
#*******************************************************************************
#*******************************************************************************

# library(ggplot2)
# library(dplyr)
# library(ade4)
# library(reshape2)
library(ggpubr)
# library(phytools)
# library(ape)

####################################################################################
#
# (1). Computing the profiles for six classes of evenness measures (Table 1 and Figure 2)
#
####################################################################################

qD <- function(p,q){
  p <- p[p>0]
  if(q!=1){
    (sum(p^q))^(1/(1-q))
  }else{
    exp(-sum(p*log(p)))
  }
}

#' new_fun computes all six classes of evenness measures.
#' @param x is an observed species abundance or frequency vector. 
#' @param q.order is a vector of diversity orders: user must specify a sequence (suggested range is from 0 to 2 in an increment of 0.05).
#' @return the profiles of all six classes of evenness indices listed in Table 1; see Figure 2 for output.
new_fun <- function(x,q.order,evenness.type,...){
  FUN <- qD
  n <- sum(x)
  p <- x/n
  q_profile_evenness <- function(q){
    qDest <- FUN(p,q)
    #S <- sum(x>0)
    S <- sum(x>0)
    E1 <- ifelse(q!=1, (1-qDest^(1-q))/(1-S^(1-q)), log(qDest)/log(S))
    E2 <- ifelse(q!=1, (1-qDest^(q-1))/(1-S^(q-1)), log(qDest)/log(S))
    E3 <- (qDest-1)/(S-1)
    E4 <- (1-1/qDest)/(1-1/S)
    E5 <- log(qDest)/log(S)
    if(q==0){
      p <- p[p>0]
      nu <- abs(p - (1/S))
      nu <- nu[nu > 0]
      sub1 <- (sum(log(abs(nu)))/sum(nu>0)-(log(1-1/S)+(1-S)*log(S))/S)
      E6 <- 1-exp(sub1)
    }else{
      p <- p[p>0]
      E6 <- 1-(sum(abs(p-1/S)^q)/((1-1/S)^q+(S-1)*S^(-q)))^(1/q)
    }
    
    #E6 <- ifelse(q=1, 1-sum(abs(p-1/S)^(1-q))/(abs(1-1/S)^(1-q)+)
    return(c(E1,E2,E3,E4,E5,E6))
  }
  out <- as.matrix(t(sapply(q.order, q_profile_evenness)))
  colnames(out) <- c("E1", "E2", "E3", "E4", "E5", "E6")
  out[,c(evenness.type)]
}



