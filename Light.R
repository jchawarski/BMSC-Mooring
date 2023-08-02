# MK9 Light tag Analysis
# project initiated August 2, 2023 by J. Chawarski, ASL Environmental Sciences

require(tidyverse)

setwd("C:/Users/jchawarski/OneDrive - ASL Environmental Sciences Inc/Projects/BMSC Summer Oceanography Course")

light <- read.csv("Data/Light Tag - MK9/2190089-Archive.csv")

# convert Time to datetime object 

# trim to underwater deployment using wet/dry setting (Dry=1 -> Dry, Dry=0 -> Wet)

# calculate solar angles for location using 'oce' package

# use solar angle to determine light anomaly -> expected light based on solar angle minus measured light level. 
# how does this impact DVM patterns seen in krill




