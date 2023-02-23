## Compare monitor with satellite PM25 estimations
## PBH
## February 2023

library(tidyverse)
library(skimr)
theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank(),axis.title.y=element_text(angle=0,margin=margin(r=-70))))


# load data ----

monitor <- read.delim("Data/pm25_month.csv",sep = ";")
satellite <- read.delim("Data/pm25Satellite_month.csv",sep=";")
skim(satellite);skim(monitor);


## join ----
df <- monitor %>% left_join(satellite) %>% na.omit() # by site, year, month

# save
write.table(df,"Data/satellite_monitor.csv",sep = ";",row.names = F)

## Correlations ------
cor(x= df$value, y= df$pm25_satellite,
    use = "complete.obs",
    method = "pearson")
cor(x= df$value, y= df$pm25_satellite,
    use = "complete.obs",
    method = "spearman")

## ranges
df$value %>% range()
df$pm25_satellite %>% range()

library(chilemapas)

## Regresion Monitor vs Satelite -------------
ggplot(df,aes(value,pm25_satellite,col=factor(year)))+
  geom_point()+
  # geom_smooth()+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  labs(x="Monitor [ug/m3]", 
       y="Satelite [ug/m3]",
       color="")

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

# EoF