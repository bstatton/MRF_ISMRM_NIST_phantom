#install packages
install.packages("tidyverse")
install.packages("broom")
install.packages("ggpmisc")


#load libraries
library(broom)
library(tidyverse)
library(ggpmisc)

#==================
#Read in data
All_data<- read.csv("data/All_spheres_rep.csv",header=T,sep = ",")#accuracy and repeatability data
all_temp <-read.csv("data/all_spheres_temp.csv",header=T,sep = ",")#temperature dependence data
ref_temp <-read.csv("data/ref_temp.csv",header=T,sep = ",")#temperature data of reference values from NIST



# Summarise accuracy and repeatbility data ----------------------------------------------------------


#mean T1 value in all spheres by each sequence 
T1_summary<-All_data %>%
  group_by(long_name) %>%
  filter(map=="T1", NIST_name!="MnCl2-1", NIST_name!="MnCl2-5")%>% #excluding spheres with odd reference values
  summarise(spheres=first(NIST_name),
            sequence=first(dictionary),
            array=first(array),
            reference_value=first(ref),
            mean=mean(tc_mean), 
            SD=sd(tc_mean),
            CV=(sd(tc_mean)/mean(tc_mean))*100,
            tc_RD_mean=mean(tc_rd),
            tc_RD_SD=sd(tc_rd))
write_csv(T1_summary, file = "Output/Summary_T1.csv")



#mean T2 value of all spheres by each sequence
T2_summary<-All_data %>%
  group_by(long_name) %>%
  filter(map=="T2", NIST_name!="MnCl2-1", NIST_name!="MnCl2-5", NIST_name!="NiCl2-1", NIST_name!="NiCl2-2", NIST_name!="NiCl2-3", NIST_name!="NiCl2-4")%>% #excluding spheres with odd reference values
  summarise(spheres=first(NIST_name),
            sequence=first(dictionary),
            array=first(array),
            reference_value=first(ref),
            mean=mean(tc_mean), 
            SD=sd(tc_mean),
            CV=(sd(tc_mean)/mean(tc_mean))*100,
            tc_RD_mean=mean(tc_rd),
            tc_RD_SD=sd(tc_rd))
write_csv(T2_summary, file = "Output/Summary_T2.csv")
 

