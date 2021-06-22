# Introduction and setting up work space ---------------------------------------------------

# Code to summarize sow posture values  
# Author: Bryanna Fayne 
# Date: June 16th, 2021

# Loading packages to set up workspace

library(readxl)
library(tidyverse)
library(gdata)
library(anytime)
library(lubridate)

# Clear workspace

rm(list = ls())

# Reading in files and defining variables  --------------------------------

# Reading in farrowing time sheet 
farr_times <- read_xlsx("data/FarrowingTimes_All_corrected.xlsx",range = "A1:G251")     # Reading in all sow farrowing times

# Making that "Stall" (sow number) column a numeric value 
farr_times$Stall <- as.numeric(farr_times$Stall)

# Timestamp in sheet is converted to R format 
farr_times$timestamp = anytime(farr_times$timestamp)
farr_times$timestamp <- force_tz(farr_times$timestamp,tzone = "UTC")              # UTC is a universal time


# Defining variables/values for the loop
lying = 0                                                                       # Counting variables
sitting = 0
standing = 0
kneeling = 0
back_to_HL<-0
drinking <- 0
udder_to_HL <-0
kneeling_down<-0
feeding <- 0

count = 0                                                      # Counter to know if it is the first row for the 15 min increment


#tnows=data.frame()
# For Loop ----------------------------------------------------------------

#pick which DS you are working with

DSnum = 2

for (sownum in 1:20) {  
  t0 <- farr_times[(farr_times$Stall == sownum)&(farr_times$DS==DSnum),]        # Reads in farrowing times
  if (is_empty(t0$`Farrowing Date`)) {next}                                     # Skipping missing values
  #Figure out the first time increment we want to count from (will be t0 - 3 days)
  if (!file.exists(paste("data/DS", DSnum,"/S", sownum, "_Sow.xlsx", sep = ""))) {
    next}
  raw = read_xlsx(paste("data/DS", DSnum,"/S", sownum, "_Sow.xlsx", sep = ""), sheet = "Raw") # Reading each sow sheet   # Skipping sow files that do not exist 
  raw = raw[order(raw$timestamp,decreasing = FALSE),] #makes sure all the rows are in order
  tbase <- t0$timestamp - (3*24*60*60)                                          #t0 - 3days
  starttime = tbase                                                              # Saves the time the first increment starts
  row =1
  inc =1
  
    while (row < dim(raw)[1]){
      #if (raw$date[row] < t0$`Farrowing Date`) {
      #  print("too soon") #not the date we are looking for, so we don't care
     # } else if (raw$date[row] == t0$`Farrowing Date`) {
      #  print("right day") 
      tnow <- raw$timestamp[row] #reads the timestamp for the current row
      
      if (tnow<tbase){
        row=row+1
      } else if (tnow>tbase+(6*24*60*60)){ #if we are past the time range we want, we need to write any remaining data and can finish this sow
          if (count>0){ #if count>0, then we have some data that we need to save
            newdata<-data.frame(DSnum,sownum, inc,starttime,lying, standing, kneeling, sitting, back_to_HL, drinking, udder_to_HL, kneeling_down, feeding,count)
            if (exists("alldata")){
              alldata<-rbind(alldata,newdata)}
            else {
              alldata<-newdata
            }
            
            
            # Reset the posture/behavior counters
            count = 0
            lying = 0 
            sitting = 0
            standing = 0
            kneeling = 0
            back_to_HL<-0
            drinking <- 0
            udder_to_HL <-0
            kneeling_down<-0
            feeding <- 0
          }
          row=dim(raw)[1] #this is the final timestamp in our desired range, so we can skip from whatever row we are on to the end of the xls
      } else if (tnow>tbase & tnow < tbase+(6*24*60*60)){ #if the row we want is within our desired +/- 3 days from farrowing period,
        
          if ((tbase+900*inc) > tnow){ #check if 15 mins (900 seconds) has elapsed since the last increment start time
            
            if (count == 0){ #if this is the first timestamp in this increment, save the timestamp
              starttime = tnow
            }
            
            
            #Put Rachel's stuff in here:
            if(raw$posture[row]==0)  # Adding standing posture
            {standing=standing + 1}
            
            if(raw$posture[row]==1)  # Adding sitting posture
            {sitting=sitting + 1}
            
            if (raw$posture[row]== 2) # Adding lying posture
            {lying=lying + 1}
            
            if (raw$posture[row]==3) # Adding kneeling posture
            {kneeling=kneeling + 1}
            
            if (!is.na(raw$attribute[row])){ #need this is.na statement because some attribute rows are blank
              
              if (raw$attribute[row]=="back to HL" | raw$attribute[row]== "back to HL other")
              {back_to_HL = back_to_HL + 1}
              
              if (raw$attribute[row]=="drinking")
              {drinking=drinking+ 1}
              
              if (raw$attribute[row]=="feeding")
              {feeding= feeding +1}  
              
              if (raw$attribute[row]== "udder to HL" | raw$attribute[row]== "udder to HL other")
              {udder_to_HL=udder_to_HL + 1} 
            }
            #else if (raw$date[row]>t0$`Farrowing Date`){
            #  print("too late") #not the date we are looking for, so we don't care
            #}
            #increase the counter
            count = count+1
            #move on to the next row (timestamp)
            row=row+1
          } else { #we have finished a 15 min period, need to save and reset our posture/behaviors
            #save all the posture/behaviors
            
            if (count>0){ #if count>0, then we have some data that we need to save
              newdata<-data.frame(DSnum,sownum, inc,starttime,lying, standing, sitting, kneeling, back_to_HL, drinking, udder_to_HL, kneeling_down, feeding,count)
              if (exists("alldata")){
                alldata<-rbind(alldata,newdata)}
              else {
                alldata<-newdata
              }
              
              
      # Reset the posture/behavior counters
              count = 0
              lying = 0 
              sitting = 0
              standing = 0
              kneeling = 0
              back_to_HL<-0
              drinking <- 0
              udder_to_HL <-0
              kneeling_down<-0
              feeding <- 0
            }
            
            # Add 1 to the increment because we are onto a new 15 min increment
            inc=inc+1
          }
      } #end while
  } #end for
  print(paste("Sow ",sownum,"is finished"))
  write.csv(alldata, "sow_behavior_sum.csv", append = TRUE)                     # Writing the data to csv file, "append" makes sure no data is overwritten 
}
  




