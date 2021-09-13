#run "Data_import.R" first

#=======
#relative deviation plot for T1 values spheres in both arrays
                                    
Pl_RD_T1<- ggplot(T1_summary, aes(x=reference_value, y=tc_RD_mean, shape = sequence, color=sequence)) + 
  geom_point(size=3,alpha=1.0, position=position_dodge(width=10))+
  geom_errorbar(aes(x=reference_value, ymin=tc_RD_mean-tc_RD_SD, ymax=tc_RD_mean+tc_RD_SD), width=.4, position=position_dodge(width=10))+
  geom_hline (yintercept=0, linetype="solid", color = "black", size = 0.5)+
  labs(title = "Relative deviation of T1 values", subtitle = "T1 array")+
  ylab("Relative deviation from reference value (%)")+
  xlab("Reference T1 Value (ms)")+
  ylim(-25,25)+
 # scale_x_discrete(labels=T1_ref_values)+
  scale_color_manual(name="sequence",
                    values=c("#00AECF", "#00C437","#FF6001"),
                    breaks=c("VFA", "MRF 1500", "MRF 3000"),
                    labels=c("VFA", "MRF 1500", "MRF 3000"))



Pl_RD_T1 + theme (plot.title = element_text(size = 16, face = "bold"),
                         text = element_text(size = 14),
                         panel.background = element_rect(fill = "white",colour = "white",size = 0.1, linetype = "solid"),
                         panel.grid.major = element_line(size = 0.1, linetype = 'solid',  colour = "#D8D8D8"), 
                         panel.grid.minor = element_line(size = 0.1, linetype = 'dashed',   colour = "#D8D8D8"),
                         axis.text = element_text(size=12),
                         axis.ticks = element_blank(),
                         axis.title = element_text(size=14),
                         panel.border = element_blank(),
                         axis.line = element_line(colour = "black"))

ggsave("plots/Pl_RD_T1.pdf", device = cairo_pdf, width = 17, height = 15, units = "cm")





# T2 Relative Deviation plots ----------------------------------------------------------------

#relative deviation plot for T2 values of spheres in both arrays
Pl_RD_T2<- ggplot(T2_summary, aes(x=reference_value, y=tc_RD_mean, shape = sequence, color=sequence)) + 
  geom_point(size=3,alpha=1.0, position=position_dodge(10))+
  geom_errorbar(aes(x=reference_value, ymin=tc_RD_mean-tc_RD_SD, ymax=tc_RD_mean+tc_RD_SD), width=.4,
                position=position_dodge(10))+
  geom_hline (yintercept=0, linetype="solid", color = "black", size = 0.5)+
  labs(title = "Relative deviation of T2 values", subtitle = "T2 array")+
  ylab("Relative deviation from reference value (%)")+
  xlab("Reference T2 Value (ms)")+
  ylim(-25,25)+
  # scale_x_discrete(labels=T1_ref_values)+
  scale_colour_manual(name="sequence",
                      values=c("#04328C", "#00C437","#FF6001"),
                      breaks=c("MESE", "MRF 1500", "MRF 3000"),
                      labels=c("MESE", "MRF 1500", "MRF 3000"))



Pl_RD_T2 + theme (plot.title = element_text(size = 16, face = "bold"),
                      text = element_text(size = 14),
                      panel.background = element_rect(fill = "white",colour = "white",size = 0.1, linetype = "solid"),
                      panel.grid.major = element_line(size = 0.1, linetype = 'solid',  colour = "#D8D8D8"), 
                      panel.grid.minor = element_line(size = 0.1, linetype = 'dashed',   colour = "#D8D8D8"),
                      axis.text = element_text(size=12),
                      axis.ticks = element_blank(),
                      axis.title = element_text(size=14),
                      panel.border = element_blank(),
                      axis.line = element_line(colour = "black"))

ggsave("plots/Pl_RD_T2.pdf", device = cairo_pdf, width = 17, height = 15, units = "cm")

