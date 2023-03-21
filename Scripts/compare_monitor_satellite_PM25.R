## Compare monitor with satellite PM25 estimations
## PBH
## February 2023

library(tidyverse)
library(skimr)
theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank(),
                              axis.title.y=element_text(angle=0,
                                                        margin=margin(r=-10))))


# load data ----

monitor <- read.delim("Data/pm25_month.csv",sep = ";")
satellite <- read.delim("Data/pm25Satellite_month.csv",sep=";")
skim(satellite);skim(monitor);


## join ----
df <- monitor %>% left_join(satellite) %>% na.omit() # by site, year, month

# save
write.table(df,"Data/satellite_monitor.csv",sep = ";",row.names = F)

## ranges
df$value %>% range()
df$pm25_satellite %>% range()

## Correlations ------
cor(x= df$value, y= df$pm25_satellite,
    use = "complete.obs",
    method = "pearson")
cor(x= df$value, y= df$pm25_satellite,
    use = "complete.obs",
    method = "spearman")


cor_region <- df %>% group_by(region) %>% 
  summarise(cor=cor(value,pm25_satellite) %>% round(2))
cor_region$year <- 2100 # total
cor_year <- df %>% group_by(year) %>% 
  summarise(cor=cor(value,pm25_satellite) %>% round(2))
cor_year$region <- "Total"
cor_total <- cor(x= df$value, y= df$pm25_satellite)


# correlation table
cor_table <- df %>% group_by(region,year) %>% 
  summarise(cor=cor(value,pm25_satellite) %>% round(2))

# add totals row and column
cor_table <- rbind(cor_table,cor_year)
cor_table <- rbind(cor_table,cor_region)

## add n
n_region <- df %>% group_by(region) %>% tally() %>% rename(cor=n)
n_region$year <- 0
n_year <- df %>% group_by(year) %>% tally() %>% rename(cor=n)
n_year$region <- "n"
  
cor_table <- rbind(cor_table,n_year)
cor_table <- rbind(cor_table,n_region)

  
# expand table
cor_table <- cor_table %>% 
  arrange(year) %>% 
  pivot_wider(names_from = year, values_from = cor) %>% 
  arrange(region)

region_order <- df %>% group_by(region) %>% summarise(latitude=mean(latitude)) %>% 
  arrange(desc(latitude)) %>% pull(region)

# order by region latitude
cor_table <- cor_table %>% arrange(factor(region, 
                                          levels = c("n",region_order,"Total")))
cor_table <- cor_table %>% rename(Total=`2100`) %>% rename(n=`0`) %>% 
  rename(Region=region)
# add totals
cor_table[18,10] <- cor_total
cor_table[18,2] <- sum(n_region$cor)
cor_table[1,10] <- sum(n_year$cor)
cor_table

# table presentation
library(flextable)
flextable(cor_table) %>% 
  autofit() %>% 
  colformat_double(digits=2) %>% 
  colformat_double(i=1,digits = 0) %>% 
  colformat_double(j=2, digits=0) %>% 
  bold(i=18) %>% bold(j=10) %>% 
  hline(i=c(1,17)) %>% vline(j=c(2,9))


# n observations in the corr table
df %>% group_by(region,year) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from = year, values_from = n)




library(chilemapas)

## Regresion Monitor vs Satelite -------------

# Add correlation to year legend
cor_year$region <- NULL
df <- df %>% 
  left_join(cor_year, by="year") %>%
  mutate(year_cor=paste0(year, " (r=",cor,")"))

p_year <- ggplot(df,aes(value,pm25_satellite,col=factor(year_cor)))+
  geom_point(alpha=.5)+
  # geom_smooth()+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  labs(x=expression(paste("PM2.5 Monitor [",mu,"g/",m^3,"]"),""), 
       y=expression(paste("PM2.5 Satellite [",mu,"g/",m^3,"]"),""), 
       color="Year (correlation)")+
  theme_bw(9)+
  theme(legend.position = c(0.8,0.17),
        panel.grid.major = element_blank(),
        legend.key.height = unit(0.1, "cm"),
        legend.box.background = element_rect(colour = "black"))
p_year

ggsave("Figures/SatelliteAcc_Year.png",p_year,
       units="cm",dpi=500,
       width = 8.7, #  1 column width
       height = 8.7)

p <- last_plot()+facet_wrap(~year)+theme(legend.position = "none")

## Add lm equation
lm_eqn <- function(df){
  m <- lm(y ~ x, data=df)
  eq <- "corr = %s \n y = %sx + %s \n R2 = %s \n N = %s \n Unc = N(%s, %s)"
  sprintf(eq, 
          format(cor(df$x,df$y, method="pearson"), digits=2),
          format(unname(coef(m)[2]), digits = 2),
          format(unname(coef(m)[1]), digits = 2),
          format(summary(m)$r.squared, digits = 3),
          format(nobs(m),digits=1),
          format(mean(summary(m)$residuals), digits=3),
          format(var(summary(m)$residuals), digits=3))
}


data_eq <- df %>% 
  dplyr::rename(x=value, y=pm25_satellite) %>% 
  dplyr::select(x,y,year)
# library(plyr)
eq <- plyr::ddply(.data = data_eq, .variables = plyr::.(year), lm_eqn)
rm(data_eq)


p_eq <- p+geom_label(data=eq,  parse = F,
                     # aes(x = 45, y = 11, label=V1),
                     aes(x = 200, y = 25, label=V1),
                     hjust="inward", size=2.8)+
  geom_smooth(method = "lm", se=T, formula = "y~x")+
   geom_point(alpha=.5)

p_eq
ggsave("Figures/Correlations_year.png", p_eq, dpi=900,
       width = 29.74, height = 18.6, units = "in")
rm(eq,p_eq,p)

# Same scatter plot but for regions

# Add correlation to year legend
cor_region$year <- NULL
df <- df %>% rename(cor_year=cor) %>% 
  left_join(cor_region, by="region") %>%
  mutate(region_cor=paste0(region, " (r=",cor,")"))

p_region <- ggplot(df,aes(value,pm25_satellite,col=factor(region_cor)))+
  geom_point(alpha=.5)+
  # geom_smooth()+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  labs(x=expression(paste("PM2.5 Monitor [",mu,"g/",m^3,"]"),""), 
       y=expression(paste("PM2.5 Satellite [",mu,"g/",m^3,"]"),""), 
       color="Region (correlation)")+
  theme_bw(9)+
  # scale_color_viridis_d()+
  guides(col=guide_legend(ncol=2))+
  theme(legend.position = c(0.77,0.17),
        panel.grid.major = element_blank(),
        legend.key.height = unit(0.1, "cm"),
        legend.spacing.x = unit(0.001,"cm"),
        legend.text = element_text(size=6),
        legend.box.background = element_rect(colour = "black"))
p_region

ggsave("Figures/SatelliteAcc_Region.png",p_region,
       units="cm",dpi=500,
       width = 8.7, #  1 column width
       height = 8.7)

library(gridExtra)

p <- grid.arrange(p_year+labs(tag="A")+theme(plot.tag.position = c(0.2,0.9)),
             p_region+labs(tag="B")+theme(plot.tag.position = c(0.2,0.9)),
             ncol=2)

ggsave("Figures/SatelliteAcc.png",p,
       units="cm",dpi=500,
       width = 8.7*2, # full width
       height = 8.7)


## Temporal correlation --------

library(ggrepel)
p_time <- ggplot(df,aes(value, pm25_satellite))+
  geom_point(alpha=.5, aes(col=year))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  facet_wrap(~site, scales="free")+
  # geom_label_repel(aes(label=year))+
  scale_color_distiller(palette = "YlOrRd", type = 'seq', na.value = "white", direction = 1)+
  coord_cartesian(xlim = c(0,70),ylim=c(0,70),expand = F)+
  labs(x="Monitor [ug/m3]", 
       y="Satelite [ug/m3]",
       color="")
p_time
ggsave("Figures/Correlations_site.png", p_time, dpi=900,
       width = 29.74, height = 18.6, units = "in")


## Time series -----
p_timeSeries <- df %>% 
  mutate(date=as.Date(paste(year,month,"01",sep="-"),"%Y-%m-%d")) %>% 
  rename(Monitor=value, Satellite=pm25_satellite) %>% 
  pivot_longer(cols=c("Monitor","Satellite")) %>% 
  ggplot(aes(date, value,col=factor(name)))+
  geom_line(aes(group=factor(name)))+
  geom_point(size=0.5)+
  facet_wrap(~site, scales="free")+
  coord_cartesian(ylim=c(0,70), expand = T)+
  labs(x="", y="MP2.5 [ug/m3]",color="")
p_timeSeries

ggsave("Figures/Correlations_timeSeries.png", p_timeSeries, dpi=900,
       width = 29.74, height = 18.6, units = "in")


## Time series by region -----
# Add correlation to region legend
cor_region$year <- NULL
df$cor <- NULL
df <- df %>% 
  left_join(cor_region, by="region") %>%
  mutate(region_cor=paste0(region, " (r=",cor,")"))

region_order <- df %>% group_by(region_cor) %>% 
  summarise(latitude=mean(latitude)) %>% 
  arrange(desc(latitude)) %>% pull(region_cor)

p_timeSeries_region <- df %>% 
  mutate(date=as.Date(paste(year,month,"01",sep="-"),"%Y-%m-%d")) %>% 
  rename(Monitor=value, Satellite=pm25_satellite) %>% 
  pivot_longer(cols=c("Monitor","Satellite")) %>%
  mutate(monitor_dummy=paste0(site,name)) %>% # dummy to unique monitor for ground and sat est.
  mutate(region_cor=factor(region_cor,levels=region_order)) %>% 
  ggplot(aes(date, value,col=factor(name)))+
  geom_line(aes(group=factor(monitor_dummy)))+
  geom_point(size=0.5)+
  facet_wrap(~region_cor, scales="free")+
  coord_cartesian(ylim=c(0,70), expand = T)+
  labs(x="", y=expression(paste("MP2.5 [",mu,"g/",m^3,"]"),""),color="",shape="")+
  theme_bw(10)+
  theme(panel.grid.major = element_blank())
p_timeSeries_region

ggsave("Figures/SatelliteAcc_Region.png",p_timeSeries_region,
       units="cm",dpi=500,
       width = 17.8, # full width
       height = 17.8/2)



## Boxplot -----
df %>% 
  rename(Monitor=value, Satellite=pm25_satellite) %>% 
  pivot_longer(cols=c("Monitor","Satellite")) %>%
  ggplot(aes(site,value,col=factor(name)))+
  geom_boxplot()+
  # facet_wrap(~region,scales="free")+
  facet_grid(region~.,scales="free",space="free")+
  coord_flip()+
  labs(x="", y="MP2.5 [ug/m3]",color="",shape="")

ggsave("Figures/Correlations_boxplots.png", last_plot(), dpi=900,
       width = 29.74, height = 18.6, units = "in")



# EoF