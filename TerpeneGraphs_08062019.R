###############
# Title: Creating Graphs for Terpene Analysis of Oil Samples.
# Author: Bianca Hoch
# Date: 08/06/2019
#
# Note: This script generates bar graphs for terpene data pertaining to Oil1.
#       Averages and standard deviations were calculated across all 10 runs.
#
#
###############

#Clear R memory, and empty garbage collector:
rm(list=ls())
gc()

#Install packages:
install.packages(c("ggplot2", "reshape2", "rio", "RColorBrewer", "readr", "dplyr"))

#Load libraries:
library(ggplot2)
library(reshape2)
library(rio)
library(RColorBrewer)
library(magrittr)
library(readr)
library(dplyr)


#Set the working directory:
setwd("C:/Users/BiancaHoch/Documents/Data/Terpene_Results/")

#-------Code Goes Below---------

#Load file of interest:
files <- list.files(pattern = ".csv", full.names = T)

#Bind the files by the Sample ID:
tbl <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "Sample")

#Save the tibble as a dataframe:
terp <- as.data.frame(tbl)

#Remove results that are "Not Determined" (ND):
terp <- subset(terp, terp$Result!="ND")

#Remove the filenames column:
terp <- terp[,2:4]

#Set values of the Results column as integers:
terp$Result <- as.numeric(as.character(terp$Result))

#Remove the triplicate numbers at the end of each sample id:
Sample <- sapply(terp$Sample.1, function(x) paste(strsplit(x, "_")[[1]][1:4], collapse = '_'))

#Add new sample ids to terp:
terp <- cbind(terp, Sample)

#Remove old sample ids:
terp <- subset(terp, select = -(Sample.1))

#Calculate the standard deviation and mean of the terpene data, grouped by sample id and terpene:
melted <- melt(terp, id.vars = c("Sample","Terpene"))
summary <- summarise(group_by(melted, Sample, Terpene, variable),
          Mean =mean(value), StDev=sd(value))
summary <- as.data.frame(summary)

#Create a bargraph with error bars:
(p5 <-  ggplot(summary, aes(x = Sample, fill = Terpene, y = Mean)) + 
        geom_bar(position = position_dodge(), stat='identity') + 
        theme_classic() +
        geom_errorbar(aes(ymin = Mean - StDev, 
                          ymax = Mean + StDev), 
                      width = 0.3, 
                      position = position_dodge(0.9)) +
        labs(title = "Average ??g/mL of Terpenes within CBD Oil Samples",
             subtitle = "Description: GC/MS terpene data with averages and standard deviations calculated across 10 runs.",
             caption = "Source: Think20 Labs") +
        ylab(" Average ??g/mL"))

#Create the faceted graph using the information from the p5 graph above:
#First, create desired color pallete:
colourCount = length(unique(summary$Terpene))
getPalette = colorRampPalette(brewer.pal(12, "PRGn"))

#Then, make the graph:
(p5 <- p5 + facet_wrap(~summary$Sample, scales = "free") + 
      theme_classic()+
      theme(strip.text.x = element_text(face = "bold"),
            axis.title.x=element_blank(), 
            axis.text.x=element_blank(), 
            axis.ticks.x=element_blank()) +
    scale_fill_manual(values = getPalette(colourCount)))

  