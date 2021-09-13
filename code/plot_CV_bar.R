#run "Data_import.R" first


# barplot coefficient of variation against reference value
#T1 values


Pl_T1_CV<- ggplot(T1_summary, aes(x=as.factor(reference_value), y=CV, fill=sequence)) + 
  geom_bar(stat="identity", width=0.4, alpha= 1, position=position_dodge(0.5))+
  ylab("CV (%)")+
  xlab("Reference T1 Value (ms)")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6))+
  scale_fill_manual(name="Sequence",
                     values=c("#00AECF", "#00C437","#FF6001"),
                      breaks=c("VFA", "MRF 1500", "MRF 3000"),
                     labels=c("VFA", "MRF 1500", "MRF 3000"))

Pl_T1_CV +  theme (plot.title = element_text(size = 16, family = "Arial", face = "bold"),
                    text = element_text(size = 12, family = "Arial"),
                    panel.background = element_rect(fill = "white",colour = "white",size = 0.1, linetype = "solid"),
                    panel.grid.major.y  = element_line(size = 0.1, linetype = 'solid',  colour = "#D8D8D8"), 
                    panel.grid.minor.y = element_line(size = 0.1, linetype = 'dashed',   colour = "#D8D8D8"),
                    axis.text = element_text(size=12, angle = 30, hjust=1) ,
                    axis.ticks = element_blank(),
                    axis.title = element_text(size=14),
                    panel.border = element_blank(),
                    axis.line = element_line(colour = "black"))

ggsave("plots/Pl_T1_CV.pdf", device = cairo_pdf, width = 17, height = 10, units = "cm")
  


#Bar plot of coefficient of variation  - T2 values in both arrays

  
Pl_T2_CV<- ggplot(T2_summary, aes(x=as.factor(reference_value), y=CV, fill=sequence)) + 
  geom_bar(stat="identity", width=0.4, alpha= 1, position=position_dodge(0.5))+
  ylab("CV (%)")+
  xlab("Reference T2 Value (ms)")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6))+
  scale_fill_manual(name="sequence",
                     values=c("#04328C", "#00C437","#FF6001"),
                     breaks=c("MESE", "MRF 1500", "MRF 3000"),
                     labels=c("MESE", "MRF 1500", "MRF 3000"))


Pl_T2_CV +  
  theme (plot.title = element_text(size = 16, family = "Arial", face = "bold"),
         text = element_text(size = 12, family = "Arial"),
         panel.background = element_rect(fill = "white",colour = "white",size = 0.1, linetype = "solid"),
         panel.grid.major.y  = element_line(size = 0.1, linetype = 'solid',  colour = "#D8D8D8"), 
         panel.grid.minor.y = element_line(size = 0.1, linetype = 'dashed',   colour = "#D8D8D8"),
         axis.text = element_text(size=12, angle = 30, hjust=1),
         axis.ticks = element_blank(),
         axis.title = element_text(size=14),
         panel.border = element_blank(),
         axis.line = element_line(colour = "black"))

ggsave("plots/Pl_T2_CV.pdf", device = cairo_pdf, width = 17, height = 10, units = "cm")
  