#*******************************************************************************
#********************************************************************************
#
#
## R scipts "Lorenz" for Chao and Ricotta (2019) Ecology paper on evenness. 
## The R code is for plotting Appendix S4: Figure S1 in Chao and Ricotta's paper.
# NOTE: The packages "ggplot2" must be installed and loaded before running the scripts. 
# 
# This code was modified by JR Junker 
#*******************************************************************************
#*******************************************************************************

library(ggplot2)

#====Functions for computing and plotting Lorenz Curve====
#'lcs_table is used to compute the values of each Lorenz curve
#' @param ps is a list of observed abundance/frequency vectors corresponding to multiple assemblages. 
#' @return the values for plotting the Lorenz curve of each assemblage.
lcs_table = function(ps, resp_var = NULL, sort_var = NULL,...){
  lc = function(p,j){
    if(is.vector(p)){
    p = p[p>0]
    s = length(p)
    p = p/sum(p)
    p = sort(p)
    out_x = sapply(1:s, function(i){i/s})
    out_y = sapply(1:s, function(i){sum(p[1:i])})
    out = data.frame(x = c(0,out_x),y = c(0,out_y),source = names(ps)[j])
    } else{ p <- data.frame(p)
    p <- p[resp_var > 0,]
    s <- nrow(p)
    resp_string = paste0("relative_",resp_var)
    # resp_var = as.name(resp_var)
    p[[resp_string]] <- with(p, resp_var/sum(resp_var)) -> p.rel
    p.rel = p.rel %>% dplyr::arrange(sort_var)
    out_x <- sapply(1:s, function(i){i/s})
    out_y <- sapply(1:s, function(i){sum(p.rel[1:i,resp_var])})
    out <- data.frame(x = c(0,out_x), y = c(0,out_y), source = names(ps)[j])
  }};debugonce(lc)
  curves = lapply(1:length(ps),function(j){lc(ps[[j]],j)})
  curves = do.call(rbind,curves)
}

#'lcs_plot is used to plot each curve
#' @param curves is the output of function lcs_table.
#' @return the plot for the Lorenz curve of each assemblage.
lcs_plot = function(curves){
  tmp = data.frame(x=c(0,1),y = c(0,1), source = "Completely even")
  curves = rbind(tmp,curves)
  curves$source = factor(curves$source, levels = unique(curves$source))
  g = ggplot(data = curves,aes(x = x,y = y,color = source, linetype = source))
  g = g + geom_line(size = 1.2)+
    geom_point(size = 3.5)+
    theme_bw()+
    theme(legend.position="bottom")+
    labs(col = "")+
    guides(linetype = FALSE)+
    ggtitle("Lorenz curve")+
    xlab("Cumulative proportion of species")+
    ylab("Cumulative proportion of abundance")+
    theme(plot.title = element_text(size=20, face="bold.italic",hjust = 0.5))
  g
}


#====Example====
#the input must be a list of observed abundance/frequency vectors corresponding to multiple assemblages
# com_1 = c(10, 2)
# com_2 = c(10, 10, 2, 1, 1)
# ps = list(com_1=com_1, com_2 = com_2)
# result_table = lcs_table(ps)
# lcs_plot(result_table)


#' This code is modified from Chao and Ricotta 2019 (Ecology)
#' source: https://github.com/AnneChao/Evenness/blob/master/Evenness.R
#' gini_even computes the two Gini evenness indices mentioned in the Discussion section and in Appendix S4.
#' 
#' @param x is an observed species abundance or frequency or production vector. 
#' @return a vector of two Gini evenness indices (non-normalized and normalizd).
gini_even <- function(x){
  x <- sort(x[x>0], decreasing = T)/sum(x)
  S <- length(x)
  ipi <- sapply(1:S, function(i) i*x[i]) %>% sum
  c("Non-normalized Gini" = (2*ipi-1)/S, "Normalized Gini" = (2*ipi-2)/(S-1))
}


#' This code is modified from ineq::Lasym
#' Lasym_strict computes the Lorenz asymmetry coefficient of a Lorenz curve with a strict ordering of values
#' e.g., not descending 
#' 
#' @param x is an observed species abundance or frequency or production vector.
#' @param ord_var is the variable by which to order x
#' @return a value of asymmetry
Lasym_strict <- function (x, resp_var = NULL, ord_var = NULL, interval = FALSE, na.rm = TRUE,...) 
{
  if (!na.rm && any(is.na(x))) 
    return(rep.int(NA_real_, 1L + as.integer(interval)))
  # x <- as.numeric(na.omit(x))
  o <- dplyr::arrange(x, ord_var)
  
  mu <- mean(resp_var, na.rm = TRUE)
  xlow <- x[resp_var < mu]
  m <- sum(w[xlow])
  n <- sum(w)
  Lm <- sum(w[xlow] * x[xlow])
  Ln <- sum(w * x)
  if (any(xeq <- x == mu)) {
    a <- sum(w[xeq])
    Lma <- sum(w[xlow | xeq] * x[xlow | xeq])
    Lac <- c(m/n + Lm/Ln, (m + a)/n + Lma/Ln)
    if (!interval) 
      Lac <- mean(Lac)
  }
  else {
    xm <- max(x[xlow])
    xm1 <- min(x[!xlow])
    delta <- (mu - xm)/(xm1 - xm)
    Lac <- (m + delta)/n + (Lm + delta * xm1)/Ln
  }
  Lac
}
