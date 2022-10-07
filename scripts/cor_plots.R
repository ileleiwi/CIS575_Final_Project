## ---------------------------
##
## Script: cor_plots
##
## Purpose: Look for variable correlation
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
library(GGally)

##Data

stroke <- read_csv("data/brain_stroke.csv")

##Histogram and Bar chart plot function
cor_numeric <- function(df){
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
  
  # Build numeric variable plot list
  scatter_plot <- function(df, var_x, var_y){
    p <- df %>%
      ggplot(aes(x = .data[[var_x]], y = .data[[var_y]])) +
      geom_point() +
      labs(x = var_x,
           y = var_y,
           title = paste(var_x, var_y, sep = "/\n")) +
      theme_bw() 
    return(p)
  }
  
  plist <- list()
  for(i in 1:length(colnames(df_n))){
    for(j in length(colnames(df_n)):1){
     plist[[length(plist)+1]] <-  scatter_plot(df = df_n, 
                                               var_x = colnames(df_n)[i], 
                                               var_y = colnames(df_n)[j])
    }
  }
  # Arrange histograms in one plot
  out_plot <- do.call("plot_grid", plist)
  return(out_plot)
}

## interested in avg_glucose_level, bmi, and age
stroke_agl_bmi_age <- stroke %>%
  mutate(stroke = ifelse(stroke == 1, "stroke", "no_stroke"),
         stroke = factor(stroke, levels = c("stroke", "no_stroke"))) %>%
  select(avg_glucose_level, bmi, age, stroke)

cor(stroke_agl_bmi_age[stroke_agl_bmi_age$stroke == "stroke",-4], 
    method = "pearson")
cor(stroke_agl_bmi_age[stroke_agl_bmi_age$stroke == "no_stroke",-4], 
    method = "pearson")

cor(stroke_agl_bmi_age[stroke_agl_bmi_age$stroke == "stroke",-4], 
    method = "spearman")
cor(stroke_agl_bmi_age[stroke_agl_bmi_age$stroke == "no_stroke",-4], 
    method = "spearman")

cor(stroke_agl_bmi_age[-4], method = "pearson")
cor(stroke_agl_bmi_age[-4], method = "spearman")


ggpairs(stroke_agl_bmi_age, ggplot2::aes(color=stroke))


pdf("figures/scatter_cor_plots.pdf", height = 10, width = 20)
cor_numeric(stroke)
dev.off()

pdf("figures/cor_plot.pdf", height = 10, width = 20)
ggpairs(stroke_agl_bmi_age, ggplot2::aes(color=stroke))
dev.off()