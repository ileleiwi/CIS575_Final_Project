## ---------------------------
##
## Script: variable_plots
##
## Purpose: Plot each variable to help determine futher analysis
##
## Author: Ikaia Leleiwi
##
## Date Created: October 7th, 2022
##
## Copyright (c) Ikaia Leleiwi, 2022
## Email: ileleiwi@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory

setwd(paste0("/Users/ikaialeleiwi/Desktop/School/Fall_2022/CIS575/",
             "Final Project/CIS575_Final_Project"))

## ---------------------------

##Libraries

library(tidyverse)

##Data

stroke <- read_csv("raw_data/brain_stroke.csv")

##Histogram and Bar chart plot function
hist_all <- function(df, type=""){
  # List of packages for session
  .packages = c("ggplot2", "cowplot")
  # Install CRAN packages (if not already installed)
  .inst <- .packages %in% installed.packages()
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
  
  
  # Filter dataframe to only numeric columns
  df_n <- df %>%
    select_if(is.numeric)
  # Filter dataframe to only character columns
  df_c <- df %>%
    select_if(is.character)
  
  # Build numeric variable plot list
  numeric_plots <- function(dn){
    nlist <- list()
    for(cat in colnames(dn)){
      p <- dn %>%
        ggplot(aes(x = .data[[cat]])) +
        geom_histogram(bins = 100) +
        labs(y = "count",
             title = cat) +
        theme_bw() +
        theme(axis.title.x = element_blank())
      nlist[[length(nlist)+1]] <- p
    }
    names(nlist) <- colnames(dn)
    return(nlist)
  }
  
  # Build character variable plot list
  character_plots <- function(dc){
    clist <- list()
    for(cat in colnames(dc)){
      p <- dc %>%
        group_by(.data[[cat]]) %>%
        count() %>%
        ggplot(aes(x = .data[[cat]])) +
        geom_bar(stat = "identity",
                 aes(y = n)) +
        labs(y = "count",
             title = cat) +
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      clist[[length(clist)+1]] <- p
    }
    names(clist) <- colnames(dc)
    return(clist)
  }
  
  #evaluate type
  if(type == "numeric"){
    plist <- numeric_plots(df_n)
  }else if(type == "character"){
    plist <- character_plots(df_c)
  }else if (type == ""){
    plist <- c(numeric_plots(df_n), character_plots(df_c))
  }
  
  # Arrange histograms in one plot
  out_plot <- do.call("plot_grid", plist)
  
  return(out_plot)
}


pdf("figures/variable_plots.pdf", height = 10, width = 10)
hist_all(stroke)
dev.off()

png("figures/variable_plots.png", height = 10, width = 10)
hist_all(stroke)
dev.off()

