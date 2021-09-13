#this code defines two functions to calculate Bland Altman statistics and plots then 
# runs those functions using the accuracy and repeatability  data of the MRF, VFA and MESE as inputs

#run "Data_import.R" first

# Define functions to create Bland Altman Statistics and plots ------------



# function to calculate Bland Altman stats for all measurements
# df is a data frame with each measurement method in a different column
# x and y are the two measurement methods to be compared by Bland Altman

bland_altman_test <- function(df,x,y) {
                                      rd_1<-((df[[x]]-df$ref)/df$ref)*100#calculate relative deviation of each method from reference value
                                      rd_2<-((df[[y]]-df$ref)/df$ref)*100
                                      diff_rd<-rd_1-rd_2
                                      mean<-(df[[x]]+df[[y]])/2
                                      mean_bias<-mean(diff_rd)
                                      UCL<-mean_bias+(1.96*(sd(diff_rd)))
                                      LCL<-mean_bias-(1.96*(sd(diff_rd)))
                                      a<-tibble(diff_rd, mean, mean_bias, UCL, LCL)
                                      
}


# function for printing a Bland Altman plot for dataframe created above
# df1 is the bland_altman_test dataframe  comparing all measurements
# df2 is the he bland_altman_test dataframe  comparing mean of all measurements
# title is the title of the saved plot


T1_BA_plot<- function(df1, df2, title) {
  ggplot()+
    geom_hline (yintercept=df1$mean_bias, linetype="solid", color = "black", size = 1)+
    geom_hline (yintercept=df1$UCL, linetype="dashed", color = "red", size = 0.5)+
    geom_hline (yintercept=df1$LCL, linetype="dashed", color = "red", size = 0.5)+
    geom_point(mapping = aes(x=df1$mean, y=df1$diff_rd),  shape = 21, size=3, fill = "black", colour = "transparent", alpha=0.3)+
    geom_point(mapping = aes(x=df2$mean, y=df2$diff_rd), colour="#00C437",size=4, alpha=0.8)+
    geom_text(aes(2600, df1$mean_bias,label = paste0 ("bias = ",sprintf("%.2f", df1$mean_bias)) , vjust=-0.6))+
    geom_text(aes(2600, df1$UCL,label = paste0 ("UCL = ",sprintf("%.2f", df1$UCL)) , vjust=-1.5))+
    geom_text(aes(2600, df1$LCL,label = paste0 ("LCL = ",sprintf("%.2f", df1$LCL)) , vjust=2))+
    labs(title = title, x="Mean T1 (ms)", y="Relative Difference in T1 %")+
    ylim(-30,10)+
    xlim(400, 2700)+
    theme (plot.title = element_text(hjust = 0.5, size = 16, family = "Arial", face = "bold"),
           text = element_text(size = 12, family = "Arial"),
           panel.background = element_rect(fill = "white",colour = "white",size = 0.5, linetype = "solid"),
           panel.grid.major = element_line(size = 0.5, linetype = 'solid',  colour = "#F2F2F2"), 
           panel.grid.minor = element_line(size = 0.25, linetype = 'solid',   colour = "#F2F2F2"),
           axis.text = element_text(size=12),
           axis.ticks = element_blank(),
           axis.title = element_text(size=14),
           panel.border = element_blank(),
           axis.line = element_line(colour = "black"))
  ggsave(paste0("plots/Pl_BA_",title,".pdf"), device = cairo_pdf, width = 15, height = 10, units = "cm")
}

T2_BA_plot<- function(df1, df2, title) {
  ggplot()+
    geom_hline (yintercept=df1$mean_bias, linetype="solid", color = "black", size = 1)+
    geom_hline (yintercept=df1$UCL, linetype="dashed", color = "red", size = 0.5)+
    geom_hline (yintercept=df1$LCL, linetype="dashed", color = "red", size = 0.5)+
    geom_point(mapping = aes(x=df1$mean, y=df1$diff_rd),  shape = 21, size=3, fill = "black", colour = "transparent", alpha=0.3)+
    geom_point(mapping = aes(x=df2$mean, y=df2$diff_rd), colour="#00C437",size=4, alpha=0.8)+
    geom_text(aes(470, df1$mean_bias,label = paste0 ("bias = ",sprintf("%.2f", df1$mean_bias)) , vjust=-0.6))+
    geom_text(aes(470, df1$UCL,label = paste0 ("UCL = ",sprintf("%.2f", df1$UCL)) , vjust=-1.5))+
    geom_text(aes(470, df1$LCL,label = paste0 ("LCL = ",sprintf("%.2f", df1$LCL)) , vjust=2))+
    labs(title = title, x="Mean T2 (ms)", y="Relative Difference in T2 %")+
    ylim(-40,40)+
    xlim(0, 500)+
    theme (plot.title = element_text(hjust = 0.5, size = 16, family = "Arial", face = "bold"),
           text = element_text(size = 12, family = "Arial"),
           panel.background = element_rect(fill = "white",colour = "white",size = 0.5, linetype = "solid"),
           panel.grid.major = element_line(size = 0.5, linetype = 'solid',  colour = "#F2F2F2"), 
           panel.grid.minor = element_line(size = 0.25, linetype = 'solid',   colour = "#F2F2F2"),
           axis.text = element_text(size=12),
           axis.ticks = element_blank(),
           axis.title = element_text(size=14),
           panel.border = element_blank(),
           axis.line = element_line(colour = "black"))
  ggsave(paste0("plots/Pl_BA_",title,".pdf"), device = cairo_pdf, width = 15, height = 10, units = "cm")
}


# Transform accuracy and repeatability data into correct format --------------------------------------

#Pivot the data to have each measurement method in a different column
BA_T1<- All_data %>% 
  filter(map=="T1", NIST_name!="MnCl2-1", NIST_name!="MnCl2-5")%>%
  select(session, dictionary, NIST_name, tc_mean, ref)%>%
  pivot_wider(
    names_from = dictionary, 
    values_from = tc_mean)

BA_T2<- All_data %>% 
  filter(map=="T2", NIST_name!="MnCl2-1", NIST_name!="MnCl2-5", NIST_name!="NiCl2-1", NIST_name!="NiCl2-2",NIST_name!="NiCl2-3",NIST_name!="NiCl2-4")%>%
  select(session, dictionary, NIST_name, tc_mean, ref)%>%
  pivot_wider(
    names_from = dictionary, 
    values_from = tc_mean)

#Create pivot dataframe of mean of all measurements for each method
BA_T1_mean<-All_data %>%
  group_by(long_name) %>%
  filter(map=="T1", NIST_name!="MnCl2-1", NIST_name!="MnCl2-5")%>%
  summarise(spheres=first(NIST_name),
            dictionary=first(dictionary),
            mean=mean(tc_mean),
            ref=first(ref))%>%
  select(spheres, dictionary, mean, ref)%>%
  pivot_wider(
    names_from = dictionary, 
    values_from = mean,
    values_fill = 0)

BA_T2_mean<-All_data %>%
  group_by(long_name) %>%
  filter(map=="T2", NIST_name!="MnCl2-1", NIST_name!="MnCl2-5", NIST_name!="NiCl2-1", NIST_name!="NiCl2-2",NIST_name!="NiCl2-3",NIST_name!="NiCl2-4")%>%
  summarise(spheres=first(NIST_name),
            dictionary=first(dictionary),
            mean=mean(tc_mean),
            ref=first(ref))%>%
  select(spheres, dictionary, mean, ref)%>%
  pivot_wider(
    names_from = dictionary, 
    values_from = mean,
    values_fill = 0)


# Run Bland Altman Analysis -----------------------------------------------


# calculate the Bland Altman stats for T1 using the function bland_altman_test created above
BA_T1_MRF1500_VFA<- bland_altman_test(BA_T1, "MRF 1500", "VFA")
BA_T1_MRF1500_VFA_mean<- bland_altman_test(BA_T1_mean, "MRF 1500", "VFA")

BA_T1_MRF3000_VFA<- bland_altman_test(BA_T1, "MRF 3000", "VFA")
BA_T1_MRF3000_VFA_mean<- bland_altman_test(BA_T1_mean, "MRF 3000", "VFA")

BA_T1_MRF1500_MRF3000<- bland_altman_test(BA_T1, "MRF 1500", "MRF 3000")
BA_T1_MRF1500_MRF3000_mean<- bland_altman_test(BA_T1_mean, "MRF 1500", "MRF 3000")

# calculate the Bland Altman stats for T2 using the function bland_altman_test created above
BA_T2_MRF1500_MESE<- bland_altman_test(BA_T2, "MRF 1500", "MESE")
BA_T2_MRF1500_MESE_mean<- bland_altman_test(BA_T2_mean, "MRF 1500", "MESE")

BA_T2_MRF3000_MESE<- bland_altman_test(BA_T2, "MRF 3000", "MESE")
BA_T2_MRF3000_MESE_mean<- bland_altman_test(BA_T2_mean, "MRF 3000", "MESE")

BA_T2_MRF1500_MRF3000<- bland_altman_test(BA_T2, "MRF 1500", "MRF 3000")
BA_T2_MRF1500_MRF3000_mean<- bland_altman_test(BA_T2_mean, "MRF 1500", "MRF 3000")



# Create Bland Altman plots -----------------------------------------------


# create Bland Altman plots for T1
T1_BA_plot(BA_T1_MRF1500_VFA,BA_T1_MRF1500_VFA_mean, "T1 MRF 1500 vs VFA")
T1_BA_plot(BA_T1_MRF3000_VFA,BA_T1_MRF3000_VFA_mean, "T1 MRF 3000 vs VFA")
T1_BA_plot(BA_T1_MRF1500_MRF3000,BA_T1_MRF1500_MRF3000_mean, "T1 MRF 1500 vs MRF 3000")
# create Bland Altman plots for T1
T2_BA_plot(BA_T2_MRF1500_MESE,BA_T2_MRF1500_MESE_mean, "T2 MRF 1500 vs MESE")
T2_BA_plot(BA_T2_MRF3000_MESE,BA_T2_MRF3000_MESE_mean, "T2 MRF 3000 vs MESE")
T2_BA_plot(BA_T2_MRF1500_MRF3000,BA_T2_MRF1500_MRF3000_mean, "T2 MRF 1500 vs MRF 3000")


