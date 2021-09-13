# this code performs the linear regression analysis on the MRF, VFA and MESE values recorded over various temperatures

#run "Data_import.R" first

#force decimal notation
options(scipen=999)


#filter by map type and by sequence
MRF1500_T1<- filter(all_temp, map == "T1" & dictionary == "MRF 1500", NIST_name!="MnCl2-1", NIST_name!="MnCl2-5")
MRF3000_T1<- filter(all_temp, map == "T1" & dictionary == "MRF 3000", NIST_name!="MnCl2-1", NIST_name!="MnCl2-5")
VFA_T1<- filter(all_temp, map == "T1" & dictionary == "VFA", NIST_name!="MnCl2-1", NIST_name!="MnCl2-5")

MRF1500_T2<- filter(all_temp, map == "T2" & dictionary == "MRF 1500", NIST_name!="MnCl2-1", NIST_name!="MnCl2-5", NIST_name!="NiCl2-1", NIST_name!="NiCl2-2", NIST_name!="NiCl2-3", NIST_name!="NiCl2-4")
MRF3000_T2<- filter(all_temp, map == "T2" & dictionary == "MRF 3000", NIST_name!="MnCl2-1", NIST_name!="MnCl2-5", NIST_name!="NiCl2-1", NIST_name!="NiCl2-2", NIST_name!="NiCl2-3", NIST_name!="NiCl2-4")
MESE_T2<- filter(all_temp, map == "T2" & dictionary == "MESE", NIST_name!="MnCl2-1", NIST_name!="MnCl2-5", NIST_name!="NiCl2-1", NIST_name!="NiCl2-2", NIST_name!="NiCl2-3", NIST_name!="NiCl2-4")





# run linear regression models for each map and each sequence --------------------------------------------

#for MRF1500 T1
by_sphere<-group_by(MRF1500_T1, NIST_name)
MRF1500_T1_rsq <- do(by_sphere, glance(lm(mean~temp, data=.)))
MRF1500_T1_coef <- do(by_sphere, tidy(lm(mean~temp, data=.)))
MRF1500_T1_coef_sp <- MRF1500_T1_coef %>% 
  select(NIST_name, term, estimate) %>%
  spread(term, estimate) 

MRF1500_T1_lm <- left_join(MRF1500_T1_coef_sp, MRF1500_T1_rsq, by="NIST_name")%>%
  add_column(sequence="MRF1500", map = "T1", .before = "NIST_name" )%>% 
  select(sequence, map, NIST_name, temp, '(Intercept)', adj.r.squared,p.value)%>%
  rename(slope = temp, intercept = '(Intercept)')


#for MRF3000 T1
by_sphere<-group_by(MRF3000_T1, NIST_name)
MRF3000_T1_rsq <- do(by_sphere, glance(lm(mean~temp, data=.)))
MRF3000_T1_coef <- do(by_sphere, tidy(lm(mean~temp, data=.)))
MRF3000_T1_coef_sp <- MRF3000_T1_coef %>% 
  select(NIST_name, term, estimate) %>%
  spread(term, estimate) 

MRF3000_T1_lm <- left_join(MRF3000_T1_coef_sp, MRF3000_T1_rsq, by="NIST_name")%>%
  add_column(sequence="MRF3000", map = "T1", .before = "NIST_name" )%>% 
  select(sequence, map, NIST_name, temp, '(Intercept)', adj.r.squared,p.value)%>%
  rename(slope = temp, intercept = '(Intercept)')


#for VFA T1
by_sphere<-group_by(VFA_T1, NIST_name)
VFA_T1_rsq <- do(by_sphere, glance(lm(mean~temp, data=.)))
VFA_T1_coef <- do(by_sphere, tidy(lm(mean~temp, data=.)))
VFA_T1_coef_sp <- VFA_T1_coef %>% 
  select(NIST_name, term, estimate) %>%
  spread(term, estimate) 

VFA_T1_lm <- left_join(VFA_T1_coef_sp, VFA_T1_rsq, by="NIST_name")%>%
  add_column(sequence="VFA", map = "T1", .before = "NIST_name" )%>% 
  select(sequence, map, NIST_name, temp, '(Intercept)', adj.r.squared,p.value)%>%
  rename(slope = temp, intercept = '(Intercept)')



#for MRF1500 T2
by_sphere<-group_by(MRF1500_T2, NIST_name)
MRF1500_T2_rsq <- do(by_sphere, glance(lm(mean~temp, data=.)))
MRF1500_T2_coef <- do(by_sphere, tidy(lm(mean~temp, data=.)))
MRF1500_T2_coef_sp <- MRF1500_T2_coef %>% 
  select(NIST_name, term, estimate) %>%
  spread(term, estimate) 

MRF1500_T2_lm <- left_join(MRF1500_T2_coef_sp, MRF1500_T2_rsq, by="NIST_name")%>%
  add_column(sequence="MRF1500", map = "T2", .before = "NIST_name" )%>% 
  select(sequence, map, NIST_name, temp, '(Intercept)', adj.r.squared,p.value)%>%
  rename(slope = temp, intercept = '(Intercept)')


#for MRF3000 T2
by_sphere<-group_by(MRF3000_T2, NIST_name)
MRF3000_T2_rsq <- do(by_sphere, glance(lm(mean~temp, data=.)))
MRF3000_T2_coef <- do(by_sphere, tidy(lm(mean~temp, data=.)))
MRF3000_T2_coef_sp <- MRF3000_T2_coef %>% 
  select(NIST_name, term, estimate) %>%
  spread(term, estimate) 

MRF3000_T2_lm <- left_join(MRF3000_T2_coef_sp, MRF3000_T2_rsq, by="NIST_name")%>%
  add_column(sequence="MRF3000", map = "T2", .before = "NIST_name" )%>% 
  select(sequence, map, NIST_name, temp, '(Intercept)', adj.r.squared,p.value)%>%
  rename(slope = temp, intercept = '(Intercept)')


#for MESE T2
by_sphere<-group_by(MESE_T2, NIST_name)
MESE_T2_rsq <- do(by_sphere, glance(lm(mean~temp, data=.)))
MESE_T2_coef <- do(by_sphere, tidy(lm(mean~temp, data=.)))
MESE_T2_coef_sp <- MESE_T2_coef %>% 
  select(NIST_name, term, estimate) %>%
  spread(term, estimate) 

MESE_T2_lm <- left_join(MESE_T2_coef_sp, MESE_T2_rsq, by="NIST_name")%>%
  add_column(sequence="MESE", map = "T2", .before = "NIST_name" )%>% 
  select(sequence, map, NIST_name, temp, '(Intercept)', adj.r.squared,p.value)%>%
  rename(slope = temp, intercept = '(Intercept)')


# Combine tables and export -----------------------------------------------

temp_lm <- bind_rows(MRF1500_T1_lm, MRF3000_T1_lm, VFA_T1_lm, MRF1500_T2_lm, MRF3000_T2_lm, MESE_T2_lm)

write_csv(temp_lm, file = "output/temp_lm.csv")
