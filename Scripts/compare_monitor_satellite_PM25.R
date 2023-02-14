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



## Regresion Monitor vs Satelite -------------
ggplot(df,aes(value,pm25_satellite,col=factor(year)))+
  geom_point()+
  geom_smooth()+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  labs(x="Monitor [ug/m3]", 
       y="Satelite [ug/m3]",
       color="")


dassda

dsad


p <- estaciones %>% 
  mutate(rm=if_else(region=="M","RM","Resto Chile") %>% factor()) %>% 
  mutate(texto=paste("Estacion: ",site,"\nComuna: ",nombre_comuna,sep="")) %>% 
  ggplot(aes(avg, avg_satelite, label=texto))+
  geom_point(alpha=.5, aes(col=rm))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  facet_wrap(~year, scales = "free")+
  coord_cartesian(xlim=c(0,70),ylim=c(0,70), expand = F)+
  labs(x="Monitor [ug/m3]", 
       y="Satelite [ug/m3]",
       color="")
p
p_int <- plotly::ggplotly(p)
mapshot(p_int,sprintf(file_name, "Correlaciones_all_interactivo_NoDust") %>% 
          str_replace("png","html"))
rm(p_int)


## Add lm equation
lm_eqn <- function(df){
  m <- lm(y ~ x, data=df)
  eq <- "corr = %s \n y = %sx + %s \n R2 = %s \n N = %s \n Unc = N(%s, %s)"
  sprintf(eq, 
          format(cor(df$x,df$y, method="pearson"), digits=2),
          format(unname(coef(m)[2]), digits = 2),
          format(unname(coef(m)[1]), digits = 2),
          format(summary(m)$r.squared, digits = 3),
          format(nobs(m),digits=0),
          format(mean(summary(m)$residuals), digits=3),
          format(var(summary(m)$residuals), digits=3))
}


data_eq <- estaciones %>% 
  dplyr::rename(x=avg, y=avg_satelite) %>% 
  dplyr::select(x,y,year)
# library(plyr)
eq <- plyr::ddply(.data = data_eq, .variables = plyr::.(year), lm_eqn)
rm(data_eq)


p_eq <- p+geom_label(data=eq,  parse = F,
                     # aes(x = 45, y = 11, label=V1),
                     aes(x = 4, y = 55, label=V1),
                     hjust="inward", size=2.8)+
  geom_smooth(method = "lm", se=T, col="black", formula = "y~x")+
  geom_point(alpha=.5, aes(col=rm))
p_eq
ggsave(sprintf(file_name,"Correlaciones_all_NoDust"), p_eq, dpi=900,
       width = 29.74, height = 18.6, units = "in")
rm(eq,p_eq,p)

## Regresion en el tiempo por monitor ------
n_estaciones <- estaciones %>% group_by(nombre_comuna,site) %>% dplyr::summarise(count=n()) %>% 
  arrange(desc(count))

p_tiempo <- estaciones %>% 
  # filter(site %in% c("Valdivia", "Las Condes","Rancagua I","Las Encinas Temuco")) %>%
  mutate(texto=paste("Estacion: ",site,"\nComuna: ",nombre_comuna,sep="")) %>% 
  mutate(year_label=if_else(year %in% seq(2000,2016,4),
                            year %>% as.character(),"")) %>% 
  ggplot(aes(avg, avg_satelite, label=texto))+
  geom_point(alpha=.5, aes(col=year))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  facet_wrap(~site, scales="free")+
  geom_label_repel(aes(label=year_label))+
  scale_color_distiller(palette = "YlOrRd", type = 'seq', na.value = "white", direction = 1)+
  coord_cartesian(xlim=c(0,70),ylim=c(0,70), expand = F)+
  labs(x="Monitor [ug/m3]", 
       y="Satelite [ug/m3]",
       color="")
p_tiempo
ggsave(sprintf(file_name,"Dispersion_Temporal_NoDust"), p_tiempo, dpi=900,
       width = 29.74, height = 18.6, units = "in")
# p_int <- plotly::ggplotly(p_tiempo)
# mapshot(p_int,sprintf(file_name, "Dispersion_Temporal_interactivo") %>% 
#           str_replace("png","html"))
rm(p_int,p_tiempo)

## Serie temporal
p_serieTiempo <- estaciones %>% 
  # filter(site %in% c("Valdivia", "Las Condes","Rancagua I","Las Encinas Temuco")) %>%
  mutate(texto=paste("Estacion: ",site,"\nComuna: ",nombre_comuna,sep="")) %>%
  rename(Monitor=avg, Satelite=avg_satelite) %>% 
  pivot_longer(cols=c("Monitor","Satelite")) %>% 
  ggplot(aes(year, value,col=factor(name), label=texto))+
  geom_line(aes(group=factor(name)))+
  geom_point(size=0.5)+
  facet_wrap(~site, scales="free")+
  coord_cartesian(ylim=c(0,70), xlim=c(2000,2016), expand = T)+
  labs(x="", y="MP2.5 [ug/m3]",color="")
p_serieTiempo

ggsave(sprintf(file_name,"SerieTemporal_NoDust"), p_serieTiempo, dpi=900,
       width = 29.74, height = 18.6, units = "in")
rm(p_serieTiempo)

# Otras pruebas
cor(estaciones$avg, estaciones$avg_satelite, method="pearson")
mod <- lm(avg_satelite~avg, estaciones)
nobs(mod)
mod <- summary(mod)
mod
mod$r.squared
mod$residuals
mod$residuals %>% mean()
mod$residuals %>% var()
rm(mod, p)


## Mapa estaciones satelite ------
m <- m1+mapview(estaciones, label=estaciones$site, col.region="red")
mapshot(m, url=sprintf(file_name, "Mapa_act_NoDust") %>% str_replace("png","html"),
        selfcontained=F)
rm(m, m1)



# EoF