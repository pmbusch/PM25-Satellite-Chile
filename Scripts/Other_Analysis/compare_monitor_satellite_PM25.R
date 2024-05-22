## Compare monitor with satellite PM25 estimations
## PBH
## February 2023

library(tidyverse)
library(skimr)
theme_set(theme_bw(10)+ theme(panel.grid.major = element_blank(),
                              axis.title.y=element_text(angle=0,
                                                        margin=margin(r=-10))))

fig_name <- "Figures/Satellite_Monitor/%s"


# load data ----

monitor <- read.delim("Data/pm25_month.csv",sep = ";")
satellite <- read.delim("Data/pm25Satellite_month.csv",sep=";")
# skim(satellite);skim(monitor);


## join ----
df <- monitor %>% left_join(satellite) %>% na.omit() # by site, year, month

# save
write.table(df,"Data/satellite_monitor.csv",sep = ";",row.names = F)


# monitor site data -----
sites <- df %>% group_by(region,province,commune,site,longitude, latitude) %>% 
  tally() %>% ungroup() %>% dplyr::select(-n)
head(sites)
write.table(df,"Data/monitor_coordinates.csv",sep = ";",row.names = F)


## ranges
df$value %>% range()
df$pm25_satellite %>% range()
df$site %>% unique()


df <- df %>%  mutate(quarter=ceiling(month/3) %>% as.integer()) %>% # quarters by months
  mutate(quarter_string=case_when(
    quarter==1 ~ "Summer", quarter==2 ~ "Fall", quarter==3 ~ "Winter", T ~ "Spring") %>% 
      factor(levels = c("Summer","Fall","Winter","Spring")))

# Ratios -----
mean(df$pm25_satellite)/mean(df$value) # 0.97
df <- df %>% mutate(ratio=pm25_satellite/value) # on average scale is the same
summary(df$ratio) # up to 6 times more, and 6 times less
quantile(summary(df$ratio),c(0.025,0.975))

df %>% group_by(year) %>% summarise(mean(value)/mean(pm25_satellite))
df %>% group_by(quarter) %>% summarise(mean(value)/mean(pm25_satellite))
df %>% group_by(month) %>% summarise(mean(value)/mean(pm25_satellite))
df %>% group_by(region) %>% summarise(mean(value)/mean(pm25_satellite))


# Valdivia 2 is wrong
# df <- df %>% filter(site!="Valdivia 2") # cor goes from .7817 to .7943

p_ratios <- ggplot(df,aes(ratio))+
  geom_histogram(bins=50)+
  scale_x_log10(breaks = c(0.2, 0.25,1/3,0.5, 1, 2, 3, 4, 5),
                labels = c("1/5", "1/4","1/3","1/2", "1", "2", "3", "4", "5"))+
  geom_vline(xintercept = 1,linetype="dashed",col="black")+
  labs(y="Freq.",x="Ratio Satellite/Monitor PM2.5", caption="Log Scale")+
  theme(panel.grid.minor = element_blank())
p_ratios

ggsave(sprintf(fig_name,"Ratio_Satellite.png"),
       units="cm",dpi=500,
       width = 8.7, #  1 column width
       height = 8.7)
# by quarter
p_ratios+facet_wrap(~quarter_string)
ggsave(sprintf(fig_name,"Ratio_Satellite_Quarter.png"),
       units="cm",dpi=500,
       width = 8.7*2, #  1 column width
       height = 8.7*2)
# by month
p_ratios+facet_wrap(~month)
# by region
p_ratios+facet_wrap(~region)
ggsave(sprintf(fig_name,"Ratio_Satellite_Region.png"),
       units="cm",dpi=500,
       width = 8.7*2, #  1 column width
       height = 8.7*2)

# Comparison of Variations within each monitor ------
names(df)
df %>% group_by(site) %>% 
  summarise(monitor=mean(value),
            satellite=mean(pm25_satellite)) %>% ungroup() %>% 
  mutate(diff=monitor-satellite, abs_diff=abs(diff)) %>% 
  arrange(desc(abs_diff)) %>% head(20)


df %>% group_by(site) %>% 
  summarise(monitor=sd(value),
            satellite=sd(pm25_satellite)) %>% ungroup() %>% 
  mutate(diff=monitor-satellite, abs_diff=abs(diff)) %>% 
  arrange(desc(abs_diff)) %>% head(20)

# histograms
df %>% 
  rename(Monitor=value,Satellite=pm25_satellite) %>% 
  pivot_longer(c(Monitor,Satellite), names_to = "key", values_to = "value") %>% 
  ggplot(aes(value,fill=key))+
  geom_histogram(alpha=.5, bins=100, position = "identity")+
  # geom_density(alpha=.5)+
  labs(x=expression(paste("PM2.5 [",mu,"g/",m^3,"]"),""),
       y="Freq.",fill="")+
  coord_cartesian(expand = F)+
  theme(legend.position = c(0.8,0.8))
ggsave(sprintf(fig_name,"Histogram.png"),
       units="cm",dpi=500,
       width = 8.7, #  1 column width
       height = 8.7)



# Correlations ------
cor(x= df$value, y= df$pm25_satellite,
    use = "complete.obs",
    method = "pearson")
cor(x= df$value, y= df$pm25_satellite,
    use = "complete.obs",
    method = "spearman")

# RMSE 
f.RMSE <- function(x,y){
  sqrt(mean((x-y)^2))
}

f.RMSE(df$value,df$pm25_satellite) # 12.7
df %>% group_by(region) %>% 
  reframe(rmse=f.RMSE(value,pm25_satellite))
df %>% group_by(year) %>% 
  reframe(rmse=f.RMSE(value,pm25_satellite))


# summary(lm(value~pm25_satellite,data=df))
# summary(lm(value~pm25_satellite-1,data=df))

# correlations without major range
df_filter <- df %>% 
  filter(value >0, value<70)
  # filter(pm25_satellite>10,pm25_satellite<50)

cor(x= df_filter$value, y= df_filter$pm25_satellite,
      use = "complete.obs",
      method = "pearson")
rm(df_filter)


df %>% group_by(year) %>% tally()
df %>% group_by(region) %>% tally()

cor_region <- df %>% group_by(region) %>% 
  summarise(cor=cor(value,pm25_satellite) %>% round(2))
cor_region$year <- 2100 # total
cor_year <- df %>% group_by(year) %>% 
  summarise(cor=cor(value,pm25_satellite) %>% round(2))
cor_year$region <- "Total"
cor_total <- cor(x= df$value, y= df$pm25_satellite)

cor_month <- df %>% group_by(month) %>% 
  summarise(cor=cor(value,pm25_satellite) %>% round(2))


df %>% group_by(site) %>% 
  summarise(cor=cor(value,pm25_satellite) %>% round(2))


cor_quarter <- df %>% group_by(quarter) %>% 
  summarise(cor=cor(value,pm25_satellite) %>% round(2))

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
library(RColorBrewer)

# Add correlation to year legend
cor_year$region <- NULL
df <- df %>% 
  left_join(cor_year, by="year") %>%
  mutate(year_cor=paste0(year, " (r=",format(cor,nsmall=2),")"))

# same scale
df$value %>% range()

set.seed(1)
p_year <- df %>% 
  sample_n(size = nrow(.), replace = FALSE) %>% # randomly reshuffle
  ggplot(aes(value,pm25_satellite,col=factor(year_cor)))+
  geom_point(alpha=.8)+
  # geom_smooth()+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  labs(x=expression(paste("",PM[2.5]," Monitor [",mu,"g/",m^3,"]"),""), 
       y=expression(paste("",PM[2.5]," Satellite [",mu,"g/",m^3,"]"),""), 
       color="Year (correlation)")+
  theme_bw(9)+
  # scale_color_manual(values = brewer.pal(7, "Blues"),limits = c(0.3, 1)) +
  scale_color_viridis_d()+
  xlim(0,205)+ylim(0,205)+
  theme(legend.position = c(0.2,0.8),
        panel.grid.major = element_blank(),
        legend.key.height = unit(0.1, "cm"),
        legend.box.background = element_rect(colour = "black"))
p_year

ggsave(sprintf(fig_name,"SatelliteAcc_Year.png"),p_year,
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
ggsave(sprintf(fig_name,"Correlations_year.png"), p_eq, dpi=900,
       width = 29.74, height = 18.6, units = "in")
rm(eq,p_eq,p)

# Same scatter plot but for regions

# Add correlation to year legend
cor_region$year <- NULL
df <- df %>% rename(cor_year=cor) %>% 
  left_join(cor_region, by="region") %>%
  # mutate(reg_length=str_length(region)) %>% 
  mutate(region_cor=paste0(region,strrep(" ",1) ,"(r=",format(cor,nsmall=2),")"))

# order by region
df <- df %>% mutate(region=factor(region,levels=region_order))
# get order for new factor
order_reg <- df %>% group_by(region,region_cor) %>% tally() %>% pull(region_cor)
df <- df %>% mutate(region_cor=factor(region_cor,levels=order_reg))

set.seed(123)
# Randomly order the dataframe for appareance
df <- df[sample(nrow(df)), ]

# see colors: https://stackoverflow.com/questions/9563711/r-color-palettes-for-many-data-classes
p_region <- ggplot(df,aes(value,pm25_satellite,col=region_cor))+
  geom_point(alpha=.6)+
  # geom_smooth()+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  labs(x=expression(paste("",PM[2.5]," Monitor [",mu,"g/",m^3,"]"),""), 
       y=expression(paste("",PM[2.5]," Satellite [",mu,"g/",m^3,"]"),""), 
       color="Region (correlation)")+
  theme_bw(9)+
  # scale_color_viridis_d(option = "turbo")+
  # ggsci::scale_color_lancet()+
  scale_color_manual(values=as.vector(pals::glasbey(16)))+
  xlim(0,205)+ylim(0,205)+
  guides(col=guide_legend(ncol=2))+
  theme(legend.position = c(0.3,0.8),
        panel.grid.major = element_blank(),
        legend.key.height = unit(0.1, "cm"),
        legend.spacing.x = unit(0.001,"cm"),
        legend.text = element_text(size=6),
        legend.box.background = element_rect(colour = "black"))
p_region

ggsave(sprintf(fig_name,"SatelliteAcc_Region.png"),p_region,
       units="cm",dpi=500,
       width = 8.7, #  1 column width
       height = 8.7)

library(gridExtra)

p <- grid.arrange(p_year+labs(tag="A")+theme(plot.tag.position = c(0.02,0.95)),
             p_region+labs(tag="B")+theme(plot.tag.position = c(0.02,0.95)),
             ncol=2)
p
ggsave(sprintf(fig_name,"SatelliteAcc.png"),p,
       units="cm",dpi=500,
       width = 8.7*2, # full width
       height = 8.7)


### By Quarter -----
# Add correlation to year legend
cor_quarter$quarter_string <- c("Summer","Fall","Winter","Spring")
df <- df %>% dplyr::select(-cor) %>% 
  left_join(cor_quarter, by="quarter") %>%
  mutate(quarter_cor=paste0(quarter_string,strrep(" ",1) ,"(r=",format(cor,nsmall=2),")"))



# get order for new factor
order_qt <- df %>% group_by(quarter,quarter_cor) %>% tally() %>% pull(quarter_cor)
df <- df %>% mutate(quarter_cor=factor(quarter_cor,levels=order_qt))


set.seed(123)
# Randomly order the dataframe for appareance
df <- df[sample(nrow(df)), ]

p_quarter <- ggplot(df,aes(value,pm25_satellite,col=quarter_cor))+
  geom_point(alpha=.5)+
  # geom_smooth()+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  labs(x=expression(paste("PM2.5 Monitor [",mu,"g/",m^3,"]"),""), 
       y=expression(paste("PM2.5 Satellite [",mu,"g/",m^3,"]"),""), 
       color="Quarter (correlation)")+
  theme_bw(9)+
  # scale_color_viridis_d(option = "turbo")+
  scale_color_manual(values = c("#FF7F27","#8B4513","#0000FF","#00AA00"))+
  xlim(0,205)+ylim(0,205)+
  guides(col=guide_legend(ncol=1))+
  theme(legend.position = c(0.3,0.8),
        panel.grid.major = element_blank(),
        legend.key.height = unit(0.1, "cm"),
        legend.spacing.x = unit(0.001,"cm"),
        legend.text = element_text(size=6),
        legend.box.background = element_rect(colour = "black"))
p_quarter

ggsave(sprintf(fig_name,"SatelliteAcc_Quarter.png"),p_quarter,
       units="cm",dpi=500,
       width = 8.7, #  1 column width
       height = 8.7)

### By Month -----
# Add correlation to year legend
cor_month
df <- df %>% dplyr::select(-cor) %>% 
  left_join(cor_month, by="month") %>%
  mutate(month_cor=paste0(month,strrep(" ",1) ,"(r=",format(cor,nsmall=2),")"))


order_m <- df %>% group_by(month,month_cor) %>% tally() %>% pull(month_cor)
df <- df %>% mutate(month_cor=factor(month_cor,levels=order_m))

set.seed(123)
# Randomly order the dataframe for appareance
df <- df[sample(nrow(df)), ]

p_month <- ggplot(df,aes(value,pm25_satellite,col=month_cor))+
  geom_point(alpha=.5)+
  # geom_smooth()+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  labs(x=expression(paste("PM2.5 Monitor [",mu,"g/",m^3,"]"),""), 
       y=expression(paste("PM2.5 Satellite [",mu,"g/",m^3,"]"),""), 
       color="Month (correlation)")+
  theme_bw(9)+
  scale_color_viridis_d(option = "turbo",direction = -1)+
  xlim(0,205)+ylim(0,205)+
  guides(col=guide_legend(ncol=2))+
  theme(legend.position = c(0.3,0.8),
        panel.grid.major = element_blank(),
        legend.key.height = unit(0.1, "cm"),
        legend.spacing.x = unit(0.001,"cm"),
        legend.text = element_text(size=6),
        legend.box.background = element_rect(colour = "black"))
p_month

ggsave(sprintf(fig_name,"SatelliteAcc_Month.png"),p_month,
       units="cm",dpi=500,
       width = 8.7, #  1 column width
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
ggsave(sprintf(fig_name,"Correlations_site.png"), p_time, dpi=900,
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

ggsave(sprintf(fig_name,"Correlations_timeSeries.png"), p_timeSeries, dpi=900,
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

ggsave(sprintf(fig_name,"SatelliteAcc_Region.png"),p_timeSeries_region,
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

ggsave(sprintf(fig_name,"Correlations_boxplots.png"), last_plot(), dpi=900,
       width = 29.74, height = 18.6, units = "in")



# EoF