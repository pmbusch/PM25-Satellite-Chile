# Income Figure for Sub sample models
#  Uses results generated in Model_Subsamples.R script
# Figure 4B
# PBH September 2023

library(tidyverse)
library(MASS)
library(lme4)
library(sandwich)

source("Scripts/Functions.R",encoding="UTF-8")

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))


# Load data generated before hand -----

res <- read.csv("Data/Models/modelResults.csv")
fig_name <- "Figures/Model/Income.png"


# Calculate RR and C.I. -----
rows <- res %>% filter(param=="pm25Exp_10ug") %>% nrow()
x <- res %>% 
  filter(param=="pm25Exp_10ug") %>%
  # filter(param=="landTemp") %>%
  mutate(rr=exp(est)*100-100,
         rr_low=exp(est-1.96*se)*100-100, # by the huge number of n, the t-stat converges to 1.96 for 5%
         rr_high=exp(est+1.96*se)*100-100) %>% 
  mutate(N=formatC(N,0, big.mark=",")) %>% 
  mutate(mean_MR=format(round(mean_MR,2),nsmall=2),
         mean_pm25=format(round(mean_pm25,2),nsmall=2),
         mean_temp=format(round(mean_temp,2),nsmall=2)) %>% 
  mutate(ci=paste0(format(round(rr,1),nsmall=1)," (",
                   format(round(rr_low,1),nsmall=1),", ",
                   format(round(rr_high,1),nsmall=1),")")) %>% 
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  mutate(rowname=1:rows) %>% 
  mutate(label=paste0(var," (n=",N,")"))


# Figure for article ------

# get base RR
rr_base <- x[1,"rr"]
rr_ci <- c(x[1,"rr_low"],x[1,"rr_high"])

# select relevant to show
cols_income <- c("Lowest income quintile (below $3,182)","2nd income quintile ($3,182-$3,577)",
               "3rd income quintile ($3,577-$4,138)","4th income quintile ($4,138-$4,961)",
               "Highest income quintile (above $4,961)")
cols_income2 <- cols_income %>% str_replace("quintile ","quintile \n")

y <- x %>% 
  filter(var %in% cols_income) %>%
  mutate(var=var %>% str_replace("quintile ","quintile \n") %>% 
           factor(levels=rev(cols_income2))) %>% 
  mutate(rr=as.numeric(rr),
         rr_low=as.numeric(rr_low),
         rr_high=as.numeric(rr_high)) %>% 
  mutate(pm25_var=str_detect(var,"PM2.5")) # special text for this


p <- y %>%
  mutate(rr_base_low=rr_ci[1],rr_base_high=rr_ci[2]) %>% 
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  ggplot(aes(x = var, y = rr)) +
  # base model
  geom_hline(yintercept = rr_base, linetype="dashed",col="brown",linewidth=0.5)+
  geom_rect(xmin=0,xmax=17,ymin = as.numeric(rr_ci[1]), ymax = as.numeric(rr_ci[2]), 
            fill = "brown",alpha=0.05)+
  geom_linerange(aes(ymin = rr_low, ymax = rr_high), linewidth = 0.2) +
  geom_point(size=1, aes(col=signif)) +
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=0.5)+
  coord_flip()+
  # coord_flip(ylim = c(-8,10))+
  # scale_y_continuous(breaks = seq(-10, 15, by = 5))+
  # scale_y_continuous(breaks = seq(-3, 0, by = 1))+ #temp
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  # annotation
  annotate("text", x = 5.2, y = 0.8, label = "Full model estimate",size=8*5/14 * 0.8) +
  geom_segment(aes(x = 5, y = 0.7, xend = 4.5, yend = 1.6),
               arrow = arrow(length = unit(0.15, "cm"))) +
  # annotate("text", x = 16, y = -12, size=14*5/14 * 0.8,label = "B")+
  # temp - uncomment
  # annotate("text", x = 5, y = rr_base-1, label = "Full model \n estimate",size=8*5/14 * 0.8) +
  # geom_segment(aes(x = 5, y = rr_base-1.2, xend = 5, yend = rr_base-0.2),
  #              arrow = arrow(length = unit(0.15, "cm"))) +
  # annotate("text", x = 16, y = -3, size=14*5/14 * 0.8,label = "B")+
  # labs(x = "Region",y =lab_rr)+
  labs(x="",y=lab_rr_temp)+
  theme_bw(8.1)+
  theme(legend.position = "none",
        # axis.title.y=element_text(angle=0,vjust = -0.05,hjust=1),
        axis.title.y=element_text(angle=0,vjust=1.015,margin=margin(r=-4)),
        panel.grid.major = element_blank(),
        # axis.title.x = element_text(size=7,hjust=1),
        panel.grid.minor = element_blank())

# Solution to draw x axis title in two lines
cowplot::ggdraw(p+labs(y=" \n "))+
  cowplot::draw_label(lab_rr_line1, x = 0.7, y = 0.07,size = 8.3)+
  cowplot::draw_label(lab_rr_line2, x = 0.7, y = 0.035,size = 8.3)
# cowplot::draw_label(lab_rr_line2_temp, x = 0.7, y = 0.035,size = 8.3)

ggsave(fig_name, ggplot2::last_plot(),
       units="cm",dpi=600,
       width=8.7,height=8.7)
# save as svg
ggsave(str_replace(fig_name,"png","svg"),ggplot2::last_plot(),
       units="cm",dpi=600,
       width = 8.7, # full width
       height =8.7)


# EoF