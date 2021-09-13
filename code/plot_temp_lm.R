# this code creates multiple plots for the linear regression analysis on the MRF, VFA and MESE values recorded over various temperatures

#run "Data_import.R" first


#filter by map 
T1_temp<- filter(all_temp, map == "T1", NIST_name!="MnCl2-1", NIST_name!="MnCl2-5")
T2_temp<- filter(all_temp, map == "T2", NIST_name!="MnCl2-1", NIST_name!="MnCl2-5", NIST_name!="NiCl2-1", NIST_name!="NiCl2-2", NIST_name!="NiCl2-3", NIST_name!="NiCl2-4")
T1_ref<- filter(ref_temp, map == "T1", NIST_name!="MnCl2-1", NIST_name!="MnCl2-5")
T2_ref<- filter(ref_temp, map == "T2", NIST_name!="MnCl2-1", NIST_name!="MnCl2-5", NIST_name!="NiCl2-1", NIST_name!="NiCl2-2", NIST_name!="NiCl2-3", NIST_name!="NiCl2-4")


# faceted plots for T1 values of all spheres ------------------------------

my.formula <- y ~ x
Pl_T1_temp  <- ggplot() +
  geom_point(data=T1_temp, aes(x = temp, y = mean, shape=dictionary, colour=dictionary), size = 1.5, alpha=0.8)+
  geom_smooth(data=T1_temp,aes(x = temp, y = mean, colour=dictionary),method="lm", se=FALSE, size=0.5, level=0.99, formula = my.formula)+
  geom_point(data=T1_ref,aes(x = temp, y = mean), size=1, alpha=0.8)+
  geom_ribbon(data=T1_ref,aes(x = temp, y = mean, ymin=(mean+stdv), ymax=(mean-stdv)), fill = "grey70", alpha=0.2) +
  ggtitle("T1 vs Temperature")+
  xlab("Temperature") + 
  ylab("T1 (ms)")+
  scale_x_continuous(breaks=c(15, 20, 25))+
  scale_shape_discrete(guide=FALSE)+
  scale_colour_manual(name="Sequence",
                      values=c("#00AECF", "#00C437","#FF6001"),
                      breaks=c("VFA", "MRF 1500", "MRF 3000"),
                      labels=c("VFA", "MRF 1500", "MRF 3000"))+
  
  
  stat_fit_glance(method="lm", label.x = 0.05, label.y= c(0.88, 0.93, 0.98), size=3,
                  method.args = list(formula = my.formula),  
                  aes(label = sprintf('R^2~"="~%.2f',stat(..adj.r.squared..))),
                  parse=TRUE) +
  
  stat_fit_glance(method = "lm", label.x=0.38, label.y=c(0.88, 0.93, 0.98), size=3,
                  method.args = list(formula = my.formula),
                  aes(label=ifelse(..p.value..< 0.01, "(p<0.01)", sprintf('(p~"="~%.2f)',
                                                                          stat(..p.value..)))),
                  parse = TRUE)



Pl_T1_temp + facet_wrap( ~ NIST_name, scales="free", ncol=4)+
  theme (plot.title = element_text (size=16, face="bold",hjust = 0.5),
         panel.background = element_rect(fill = "white",colour = "white",size = 0.1, linetype = "solid"),
         panel.grid.major  = element_line(size = 0.1, linetype = 'dashed',  colour = "#D8D8D8"), 
         axis.line = element_line(colour = "black"),
         text = element_text(size = 12, family = "Arial"),
         axis.title = element_text(face="bold", size = 16 ),
         axis.text =element_text(size = 12),
         legend.text = element_text( size=14),
         legend.title = element_text(size=16, face="bold"),
         strip.text.x = element_text(size=10),
         strip.background = element_rect(colour="gray", fill="gray"))

ggsave("plots/Pl_T1_temp.pdf", device = cairo_pdf, width = 30, height = 21, units = "cm")


#temp correlation plots for T2 in all spheres

my.formula <- y ~ x
Pl_T2_temp  <- ggplot() +
  geom_point(data=T2_temp, aes(x = temp, y = mean, shape=dictionary, colour=dictionary), size = 1.5, alpha=0.8)+
  geom_smooth(data=T2_temp,aes(x = temp, y = mean, colour=dictionary),method="lm", se=FALSE, size=0.5, level=0.99, formula = my.formula)+
  geom_point(data=T2_ref,aes(x = temp, y = mean), size=1, alpha=0.8)+
  geom_ribbon(data=T2_ref,aes(x = temp, y = mean, ymin=(mean+stdv), ymax=(mean-stdv)), fill = "grey70", alpha=0.2) +
  ggtitle("T2 vs Temperature")+
  xlab("Temperature") + 
  ylab("T2 (ms)")+
  scale_x_continuous(breaks=c(15, 20, 25))+
  scale_shape_discrete(guide=FALSE)+
  scale_colour_manual(name="Sequence",
                      values=c("#255699", "#00C437","#FF6001"),
                      breaks=c("MESE", "MRF 1500", "MRF 3000"),
                      labels=c("MESE", "MRF 1500", "MRF 3000"))+
  
  
  stat_fit_glance(method="lm", label.x = 0.05, label.y= c(0.89, 0.94, 0.99), size=3,
                  method.args = list(formula = my.formula),  
                  aes(label = sprintf('R^2~"="~%.3f',stat(..r.squared..))),
                  parse=TRUE) +
  
  stat_fit_glance(method = "lm", label.x=0.31, label.y=c(0.89, 0.94, 0.99), size=3,
                  method.args = list(formula = my.formula),
                  aes(label=ifelse(..p.value..< 0.001, "(p<0.001)", sprintf('(p~"="~%.3f)',
                                                                            stat(..p.value..)))),
                  parse = TRUE)



Pl_T2_temp + facet_wrap( ~ NIST_name, scales="free", ncol=4)+
  theme (plot.title = element_text (size=16, face="bold",hjust = 0.5),
         panel.background = element_rect(fill = "white",colour = "white",size = 0.1, linetype = "solid"),
         panel.grid.major  = element_line(size = 0.1, linetype = 'dashed',  colour = "#D8D8D8"), 
         axis.line = element_line(colour = "black"),
         text = element_text(size = 12, family = "Arial"),
         axis.title = element_text(face="bold", size = 16 ),
         axis.text =element_text(size = 12),
         legend.text = element_text( size=14),
         legend.title = element_text(size=16, face="bold"),
         strip.text.x = element_text(size=10),
         strip.background = element_rect(colour="gray", fill="gray"))

ggsave("plots/Pl_T2_temp.pdf", device = cairo_pdf, width = 30, height = 15, units = "cm")

