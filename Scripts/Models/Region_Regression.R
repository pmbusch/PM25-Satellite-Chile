## Regressions at Region regression Level
## PBH
## June 2023

library(tidyverse)
library(MASS)
library(lme4)
library(sandwich)


theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))

source("Scripts/Functions.R",encoding="UTF-8")

# Load required data -----
df <- read.delim("Data/Panel Data/panelData.csv",sep=";")

# Change some parameters
df <- df %>% 
  mutate(quarter=ceiling(as.numeric(month)/3) %>% factor()) %>% 
  mutate(year_quarter=paste0(year,"-",quarter)) %>% 
  mutate(year=as.factor(year),
         year_quarter=as.factor(year_quarter),
         region=as.factor(REGION),
         commune=as.factor(codigo_comuna),
         month=as.factor(month),
         quarter=as.factor(quarter))

# Base model ----
mod_base <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+year_quarter+
                     offset(log(pop75)), na.action=na.omit,
                   data = df) 
rr_base <-  getModelInfo(mod_base,"Base") %>% 
  filter(param=="pm25Exp_10ug") %>% pull(rr)

# Loop --------
regs <- df$region %>% unique()
# regs <- df$commune %>% unique()
# regs <- df$PROVINCIA %>% unique()
all_mods <- data.frame()

# one model for each commune
for (x in regs){
  
  n_com <- filter(df,commune==x) %>% pull(commune) %>% unique() %>% length()
  
  if(n_com==1){
    mod <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year_quarter+
                    offset(log(pop75)), na.action=na.omit,
                  data = filter(df,region==x)) # filter by region
                  # data = filter(df,commune==x)) # filter by comm
                  # data = filter(df,PROVINCIA==x)) # by prov
  } else{
  mod <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+year_quarter+
                  offset(log(pop75)), na.action=na.omit,
                data = filter(df,region==x)) # filter by region
                # data = filter(df,commune==x)) # filter by comm
                # data = filter(df,PROVINCIA==x)) # by prov
  }
  

  out <- getModelInfo(mod,x,data_df =filter(df,region==x))
  # out <- getModelInfo(mod,x,data_df =filter(df,commune==x)) # by comm
  # out <- getModelInfo(mod,x,data_df =filter(df,PROVINCIA==x)) # by prov
  all_mods <- rbind(all_mods,out)
  rm(out)
}

write.csv(all_mods,"Data/Models/modelResults_region.csv",row.names = F)
all_mods <- read.csv("Data/Models/modelResults_region.csv")


# Figure ------

param_int <- "pm25Exp_10ug"
# param_int <- "landTemp"  # temp

df_fig <- all_mods %>% 
  filter(param==param_int)

rr_base <-  getModelInfo(mod_base,"Base") %>% 
  filter(param==param_int) %>% pull(rr)
rr_ci <- getModelInfo(mod_base,"Base") %>% 
  filter(param==param_int) %>% dplyr::select(rr_low,rr_high)

region_levels <- c("15","1","2","3","4","5","M","6","7","16","8","9","14","10","11","12")
region_levels2 <- c("XV","I","II","III","IV","V","M","VI","VII","XVI","VIII","IX","XIV","X","XI","XII")

reg_codes <- data.frame(name=region_levels, region=region_levels2)

# Change regions name
df_fig <- df_fig %>%
  mutate(name=name %>% str_replace("13","M")) %>% 
  left_join(reg_codes, by="name") %>% 
  mutate(region=factor(region,levels=rev(region_levels2))) %>% 
  mutate(n_commune=N/12/18)

# Figure
p <- df_fig %>%
  mutate(rr_base_low=rr_ci[1],rr_base_high=rr_ci[2]) %>% 
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  ggplot(aes(x = region, y = rr)) +
  # base model
  geom_hline(yintercept = rr_base, linetype="dashed",col="brown",linewidth=0.5)+
  geom_rect(xmin=0,xmax=17,ymin = as.numeric(rr_ci[1]), ymax = as.numeric(rr_ci[2]), 
            fill = "brown",alpha=0.01)+
  # geom_text(y=-12.5,aes(label=n_commune),size=8.3*5/14 * 0.8)+
  geom_linerange(aes(ymin = rr_low, ymax = rr_high), linewidth = 0.2) +
  geom_point(size=1, aes(col=signif)) +
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=0.5)+
  coord_flip()+
  # coord_flip(ylim = c(-8,10))+
  scale_y_continuous(breaks = seq(-10, 15, by = 5))+
  # scale_y_continuous(breaks = seq(-3, 0, by = 1))+ #temp
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  # annotation
  annotate("text", x = 1, y = rr_base+9.5, label = "Full model estimate",size=8*5/14 * 0.8) +
  geom_segment(aes(x = 1, y = rr_base+4.2, xend = 1, yend = rr_base+1.3),
               arrow = arrow(length = unit(0.15, "cm"))) +
  # annotate("text", x = 16, y = -12, size=14*5/14 * 0.8,label = "B")+
  # temp - uncomment
  # annotate("text", x = 5, y = rr_base-1, label = "Full model \n estimate",size=8*5/14 * 0.8) +
  # geom_segment(aes(x = 5, y = rr_base-1.2, xend = 5, yend = rr_base-0.2),
  #              arrow = arrow(length = unit(0.15, "cm"))) +
  # annotate("text", x = 16, y = -3, size=14*5/14 * 0.8,label = "B")+
  # labs(x = "Region",y =lab_rr)+
  labs(x="Region",y=lab_rr_temp)+
  theme_bw(8.3)+
  theme(legend.position = "none",
        # axis.title.y=element_text(angle=0,vjust = -0.05,hjust=1),
        axis.title.y=element_text(angle=0,vjust=1.015,margin=margin(r=-14)),
        panel.grid.major = element_blank(),
        # axis.title.x = element_text(size=7,hjust=1),
        panel.grid.minor = element_blank())

# Solution to draw x axis title in two lines
cowplot::ggdraw(p+labs(y=" \n "))+
  cowplot::draw_label(lab_rr_line1, x = 0.5, y = 0.07,size = 8.3)+
  cowplot::draw_label(lab_rr_line2, x = 0.5, y = 0.035,size = 8.3)
# cowplot::draw_label(lab_rr_line2_temp, x = 0.5, y = 0.035,size = 8.3)

fig_name <- "RegionModels"
# fig_name <- "RegionModelsTemp"
ggsave(paste0("Figures/Model/",fig_name,".png"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

# save as svg
ggsave(paste0("Figures/Model/",fig_name,".svg"),ggplot2::last_plot(),
       units="cm",dpi=500,
       width = 8.7, # full width
       height =8.7)



# Average effect of sign. regions
df_fig %>%
  mutate(signif=sign(rr_low)==sign(rr_high)) %>% 
  filter(region !="XII") %>% 
  group_by(signif) %>% 
  summarise(rr=mean(rr)) #4.8%


## Average exposure by region ------
df_stats <- df %>% 
  mutate(region=region %>% str_replace("13","M")) %>%
  left_join(reg_codes, by=c("region"="name")) %>% 
  mutate(region=factor(region,levels=rev(region_levels2))) %>% 
  mutate(pm25_pop=pop75*pm25_exposure,
         share_pop=pop75*pop75_share) %>% 
  group_by(region.y) %>% 
  summarise(pm25=sum(pm25_pop),
            share75=sum(share_pop),
            pop75=sum(pop75),
            death=sum(death_count_all_cause),
            n=n()) %>% ungroup() %>% 
  rename(region=region.y) %>% 
  mutate(pm25=pm25/pop75,
         share75=share75/pop75,
         mr=death/pop75*1000,
         pop75=pop75/18/12) # pop in total


## Mortality rate by region -----

# Mortality rate
a <- df %>% 
  mutate(region=region %>% str_replace("13","M")) %>%
  rename(region1=region) %>% 
  left_join(reg_codes, by=c("region1"="name")) %>% 
  mutate(region=factor(region,levels=rev(region_levels2))) %>% 
  # group_by(region,commune) %>% 
  # group_by(region,year) %>%
  group_by(region) %>%
  reframe(pop75=sum(pop75),death_count_all_cause=sum(death_count_all_cause)) %>% 
  ungroup() %>% 
  # mutate(date=as.Date(paste(year,"01","01",sep="-"),"%Y-%m-%d")) %>% 
  mutate(MR_all_cause=death_count_all_cause/pop75*1e3) %>% 
  arrange(desc(region))
a
a %>% 
  mutate(labe=paste0(round(MR_all_cause,1),"")) %>% 
  ggplot(aes(region,MR_all_cause,fill=region))+
  geom_col()+
  geom_text(aes(label=labe))+
  # facet_wrap(~year)+
  # facet_grid(region~.,scales = "free_y",space = "free")+
  coord_flip()

ggplot(a,aes(date,MR_all_cause,col=fct_rev(region),group=fct_rev(region)))+
  geom_line()+
  geom_text(data=filter(a,year==2019),aes(label=region),
            nudge_x = 2)+
  scale_color_viridis_d(option = "turbo")


## IDEA: Create table as other sub samples
## Scatter ----
p <- df_fig %>%
  # dplyr::select(-region) %>% 
  # rename(region=name) %>% 
  left_join(df_stats) %>% 
  mutate(region=paste0("",region)) %>% 
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  ggplot(aes(x = pm25, y = rr)) +
  geom_hline(yintercept = rr_base, linetype="dashed",col="brown",linewidth=0.5)+
  geom_rect(xmin=0,xmax=35,ymin = as.numeric(rr_ci[1]), ymax = as.numeric(rr_ci[2]), 
            fill = "brown",alpha=0.01)+
  geom_linerange(aes(ymin = rr_low, ymax = rr_high), linewidth = 0.2) +
  geom_point(size=1, aes(col=signif))+
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=0.5)+
  # ggrepel::geom_text_repel(aes(label=region),size=6*5/14 * 0.8,max.time=10)+
  geom_text(aes(label=region),size=6*5/14 * 0.8,
            nudge_x = c(-0.5,0.6,-0.6,0.7,-0.55,
                        -0.65,0.75,-0.8,0.7,-0.5,
                        -0.65,-0.75,-0.5,-0.85,0.75,0.9),
            nudge_y = c(0,0,0,0,-0.5,
                        0,0.5,0,0,0,
                        1,0,0.5,0.5,-0.5,0)
            
            )+
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  # annotation
  scale_y_continuous(breaks = seq(-10, 15, by = 5))+
  annotate("text", x = 8, y = rr_base+7, label = "Full model \n estimate",size=8*5/14 * 0.8, hjust=0) +
  geom_segment(aes(x = 10, y = rr_base+5.2, xend = 10, yend = rr_base+1.2),
               arrow = arrow(length = unit(0.3, "cm"))) +
  labs(x =expression(paste("Average monthly ",PM[2.5]," [",mu,"g/",m^3,"]","")),
       y = lab_rr)+
  theme_bw(10)+
  theme(legend.position = "none",
        axis.title.y = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Solution to draw x axis title in two lines
cowplot::ggdraw(p+labs(y=" \n "))+
  cowplot::draw_label(lab_rr_line1, y = 0.5, x = 0.035,size = 8.3,angle = 90)+
  cowplot::draw_label(lab_rr_line2, y = 0.5, x = 0.07,size = 8.3,angle = 90)
# cowplot::draw_label(lab_rr_line2_temp, y = 0.5, x = 0.07,size = 8.3,angle=90)

ggsave("Figures/Model/RegionModelsPM25.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)
# save as svg
ggsave("Figures/Model/RegionModelsPM25.svg",ggplot2::last_plot(),
       units="cm",dpi=500,
       width = 8.7, # full width
       height =8.7)
pdf("Figures/Model/RegionModelsPM25.pdf",
       width = 8.7/2.54, # full width
       height =8.7/2.54)
ggplot2::last_plot()
dev.off()


## Scatter vs share of pop75+ in region --------
p <- df_fig %>%
  left_join(df_stats) %>% 
  mutate(region=paste0("",region)) %>% 
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  ggplot(aes(x = share75   , y = rr)) +
  geom_hline(yintercept = rr_base, linetype="dashed",col="brown",linewidth=0.5)+
  geom_rect(xmin=0,xmax=35,ymin = as.numeric(rr_ci[1]), ymax = as.numeric(rr_ci[2]), 
            fill = "brown",alpha=0.01)+
  geom_linerange(aes(ymin = rr_low, ymax = rr_high), linewidth = 0.2) +
  geom_point(size=1, aes(col=signif))+
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=0.5)+
  # ggrepel::geom_text_repel(aes(label=region),size=6*5/14 * 0.8,max.time=10)+
  geom_text(aes(label=region),size=6*5/14 * 0.8,
            nudge_x = c(-0.5,0.6,-0.6,-0.7,0.55,
                        -0.65,-0.5,-0.8,0.7,0.5,
                        -0.65,-0.75,0.45,0.85,-0.75,-0.9)*0.001,
            nudge_y = c(0,0,0,0,-0.5,
                        0,0.6,0,0,0,
                        1,0,0.5,0.5,-0.5,0)
  )+
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  # annotation
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(breaks = seq(-10, 15, by = 5))+
  annotate("text", x = 0.037, y = rr_base+7, label = "Full model \n estimate",size=8*5/14 * 0.8, hjust=0) +
  geom_segment(aes(x = 0.0385, y = rr_base+5.2, xend = 0.0385, yend = rr_base+1.2),
               arrow = arrow(length = unit(0.3, "cm"))) +
  labs(x ="Share of population above 75 years",
       y = lab_rr)+
  theme_bw(10)+
  theme(legend.position = "none",
        axis.title.y = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Solution to draw x axis title in two lines
cowplot::ggdraw(p+labs(y=" \n "))+
  cowplot::draw_label(lab_rr_line1, y = 0.5, x = 0.035,size = 8.3,angle = 90)+
  cowplot::draw_label(lab_rr_line2, y = 0.5, x = 0.07,size = 8.3,angle = 90)
# cowplot::draw_label(lab_rr_line2_temp, y = 0.5, x = 0.07,size = 8.3,angle=90)

ggsave("Figures/Model/RegionModels_share75.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)
# save as svg
ggsave("Figures/Model/RegionModels_share75.svg",ggplot2::last_plot(),
       units="cm",dpi=500,
       width = 8.7, # full width
       height =8.7)
pdf("Figures/Model/RegionModels_share75.pdf",
    width = 8.7/2.54, # full width
    height =8.7/2.54)
ggplot2::last_plot()
dev.off()



## Curve by commune ----
# NEED TO RUN 327 MODELS, one for each commune


df_stats2 <- df %>% 
  mutate(pm25_pop=pop75*pm25_exposure) %>% 
  group_by(commune) %>%
  # group_by(PROVINCIA) %>% 
  summarise(pm25=sum(pm25_pop),
            pop75=sum(pop75),
            death=sum(death_count_all_cause),
            n=n()) %>% ungroup() %>% 
  mutate(pm25=pm25/pop75,
         mr=death/pop75*1000,
         pop75=pop75/18/12) # pop in total

df_fig %>%
  mutate(signif=sign(rr_low)==sign(rr_high)) %>% 
  group_by(signif) %>% 
  summarise(rr=mean(rr)) #4.8%

df_fig %>%
  rename(commune=name) %>%
  # rename(PROVINCIA=name) %>% 
  left_join(df_stats2) %>% 
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  ggplot(aes(x = pm25, y = rr)) +
  geom_hline(yintercept = rr_base, linetype="dashed",col="brown",linewidth=0.5)+
  geom_rect(xmin=0,xmax=40,ymin = as.numeric(rr_ci[1]), ymax = as.numeric(rr_ci[2]), 
            fill = "brown",alpha=0.01)+
  geom_linerange(aes(ymin = rr_low, ymax = rr_high), linewidth = 0.2) +
  geom_point(size=1, aes(col=signif))+
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=0.5)+
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  # annotation
  annotate("text", x = 8, y = rr_base+7, label = "Full model estimate",size=8*5/14 * 0.8, hjust=0) +
  geom_segment(aes(x = 10, y = rr_base+5.2, xend = 10, yend = rr_base+1.2),
               arrow = arrow(length = unit(0.3, "cm"))) +
  # stat_smooth(aes(col=signif),method = "lm", formula = y ~ x + I(x^2), 
  #             linewidth = 0.5)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), 
              linewidth = 0.5)+
  labs(x =lab_pm25,
       y = lab_rr)+
  coord_cartesian(ylim=c(-70,100))+
  # coord_cartesian(ylim=c(-30,30))+
  theme_bw(10)+
  theme(legend.position = "none",
        axis.title.y = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("Figures/Model/CommuneModelsPM25.png", ggplot2::last_plot(),
# ggsave("Figures/Model/ProvinceModelsPM25.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)


# Scatter plot PM2.5 and T ----

# test figure
df_fig <- all_mods %>% 
  mutate(name=name %>% str_replace("13","M")) %>% 
  left_join(reg_codes, by="name") %>% 
  mutate(region=factor(region,levels=rev(region_levels2))) %>% 
  # mutate(rr=est) %>% # to compute correlation easily
  dplyr::select(region,param,rr,rr_low,rr_high) %>% 
  filter(param %in% c("pm25Exp_10ug","landTemp")) %>% 
  # pivot_longer(c(rr,rr_low,rr_high), names_to = "level", values_to = "value") %>% 
  pivot_wider(names_from = param, values_from = c(rr,rr_low,rr_high))

names(df_fig)
cor(df_fig$rr_pm25Exp_10ug,df_fig$rr_landTemp) # 0.29
ggplot(df_fig,aes(rr_pm25Exp_10ug,rr_landTemp))+
  geom_errorbar(aes(ymin = rr_low_landTemp, ymax = rr_high_landTemp)) +
  geom_errorbarh(aes(xmin = rr_low_pm25Exp_10ug, xmax = rr_high_pm25Exp_10ug))+
  # geom_smooth(method=lm,se=F)+
  geom_point(col="red")+
  ggrepel::geom_text_repel(aes(label=region))+
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=0.5)+
  geom_vline(xintercept = 0, linetype="dashed",col="grey",linewidth=0.5)+
  labs(x=lab_rr,y=expression(paste("Percentage change in Mortality rate by 1° Celsius")))+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())

ggsave("Figures/Model/Region_Scatter.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7*2,height=8.7*2)


df %>% group_by(REGION) %>% summarise(temp=mean(landTemp))
# EoF