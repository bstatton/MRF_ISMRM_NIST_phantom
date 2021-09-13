#run "Data_import.R" first




##facet plot correlatons between measured T1 and T2 values and the NIST reference values============
#T1 Values 

my.formula <- y ~ x
T1_cor_plot  <- ggplot(T1_summary, aes(x = reference_value, y = mean, shape=sequence, colour=sequence)) +
  geom_point(size = 3) +
  geom_smooth(method="lm", se=FALSE, size=0.5, level=0.99, formula = my.formula)+
  xlab("Reference T1 (ms)") + 
  ylab("Measured T1 (ms)")+
  #xlim(0,2000)
  #ylim(0,2000)
  scale_y_continuous(limits=c(1, 2500))+
  scale_x_continuous(limits=c(1, 2500))+
  scale_shape_discrete(guide=FALSE)+
  scale_colour_manual(name="Sequence",
                      values=c("#00AECF", "#00C437","#FF6001"),
                      breaks=c("VFA", "MRF 1500", "MRF 3000"),
                      labels=c("VFA", "MRF 1500", "MRF 3000"))+
  stat_fit_glance(method="lm", label.x = 0.05, label.y= c(0.97), size=5,
                  method.args = list(formula = my.formula),  
                  aes(label = sprintf('R^2~"="~%.3f',stat(..adj.r.squared..))),
                  parse=TRUE) +
  
  stat_fit_glance(method = "lm", label.x=0.05, label.y=c(0.9), size=5,
                  method.args = list(formula = my.formula),
                  aes(label=ifelse(..p.value..< 0.001, "(p<0.001)", sprintf('(p~"="~%.3f)',
                                                                            stat(..p.value..)))),
                  parse = TRUE)+ 
  stat_poly_eq(formula = my.formula, label.x=0.05, label.y=c(0.83), size=5,
               aes(label = paste(..eq.label.., sep = "~~~")), 
               parse = TRUE)




T1_cor_plot + facet_wrap( ~ sequence, scales="free", nrow=1)+
  theme (plot.title = element_text (size=16, face="bold",hjust = 0.5),
         text = element_text(size = 12, family = "Arial"),
         panel.background = element_rect(fill = "white",colour = "white",size = 0.1, linetype = "solid"),
         panel.grid.major  = element_line(size = 0.1, linetype = 'dashed',  colour = "#D8D8D8"), 
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 16 ),
         axis.text =element_text(size = 14),
         legend.text = element_text( size=14),
         legend.title = element_text(size=16, face="bold"),
         strip.text.x = element_text(size=10),
         strip.background = element_rect(colour="gray", fill="gray"))

ggsave("plots/Pl_T1_cor.pdf", device = cairo_pdf, width = 31, height = 10, units = "cm")


#============
#T2 Values

my.formula <- y ~ x
T2_cor_plot  <- ggplot(T2_summary, aes(x = reference_value, y = mean, shape=sequence,colour=sequence)) +
  geom_point(size = 3) +
  geom_smooth(method="lm", se=FALSE, size=0.5, level=0.99, formula = my.formula)+
  xlab("Reference T2 (ms)") + 
  ylab("Measured T2 (ms)")+
  scale_y_continuous(limits=c(1, 400))+
  scale_x_continuous(limits=c(1, 400))+
  scale_shape_discrete(guide=FALSE)+
  scale_colour_manual(name="Sequence",
                      values=c("#04328C", "#00C437","#FF6001"),
                      breaks=c("MESE", "MRF 1500", "MRF 3000"),
                      labels=c("MESE", "MRF 1500", "MRF 3000"))+
  stat_fit_glance(method="lm", label.x = 0.05, label.y= c(0.97), size=5,
                  method.args = list(formula = my.formula),  
                  aes(label = sprintf('R^2~"="~%.4f',stat(..adj.r.squared..))),
                  parse=TRUE) +
  
  stat_fit_glance(method = "lm", label.x=0.05, label.y=c(0.9), size=5,
                  method.args = list(formula = my.formula),
                  aes(label=ifelse(..p.value..< 0.001, "(p<0.001)", sprintf('(p~"="~%.3f)',
                                                                            stat(..p.value..)))),
                  parse = TRUE)+ 
  stat_poly_eq(formula = my.formula, label.x=0.05, label.y=c(0.83), size=5,
               aes(label = paste(..eq.label.., sep = "~~~")), 
               parse = TRUE)




T2_cor_plot + facet_wrap( ~ sequence, scales="free", nrow=1)+
  theme (plot.title = element_text (size=16, face="bold",hjust = 0.5),
         text = element_text(size = 12, family = "Arial"),
         panel.background = element_rect(fill = "white",colour = "white",size = 0.1, linetype = "solid"),
         panel.grid.major  = element_line(size = 0.1, linetype = 'dashed',  colour = "#D8D8D8"), 
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 16 ),
         axis.text =element_text(size = 14),
         legend.text = element_text( size=14),
         legend.title = element_text(size=16, face="bold"),
         strip.text.x = element_text(size=10),
         strip.background = element_rect(colour="gray", fill="gray"))

ggsave("plots/Pl_T2_cor.pdf", device = cairo_pdf, width = 31, height = 10, units = "cm")
