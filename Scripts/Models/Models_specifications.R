## Different Models comparisons by Model specification
## PBH
## March 2023

library(tidyverse)
library(MASS)
library(lme4)
library(sandwich)

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))

source("Scripts/Functions.R",encoding="UTF-8")


# Load required data -----
df <- read.delim("Data/Panel Data/panelData.csv",sep=";")

df <- df %>% filter(!is.na(pm25Exp_10ug))

df <- df %>% 
  mutate(year_quarter=paste0(year,"-",quarter)) %>% 
  mutate(quarter=factor(quarter),
         year=as.factor(year),
         year_quarter=as.factor(year_quarter),
         region=as.factor(REGION),
         month=as.factor(month),
         commune=as.factor(codigo_comuna),
         commune=relevel(commune,ref="13101")) # Santiago


## Negative Binomial forms ----
# year+quarter
model_nb <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year+quarter+commune+
                     offset(log(pop75)), 
                   data = df,
                   na.action=na.omit)
models_nb_res <- getModelInfo(model_nb,"Year+Quarter")


# year*month
mod_nb1 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year*month+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb1,"Year*Month"))

# year+month
mod_nb2 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year+month+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb2,"Year+Month"))

# year+quarter (no month)
mod_nb3 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year+quarter+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb3,"Year+Quarter"))

# year*quarter (no month)
mod_nb4 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year*quarter+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb4,"Year*Quarter"))

# year*quarter + month
mod_nb5 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year*quarter+month+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb5,"Year*Quarter+Month"))

# year+quarter+month
mod_nb6 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year+quarter+month+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb6,"Year+Quarter+Month"))

# region*quarter
mod_nb7 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year+region*quarter+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb7,"Region*Quarter"))

mod_nb7a <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year*month+region*quarter+commune+
                    offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb7a,"Region*Quarter+Year*Month"))

mod_nb7b <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year+month+region*quarter+commune+
                    offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb7b,"Region*Quarter+Year+Month"))


# region*year+quarter
mod_nb8 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+region*year+quarter+commune+
                    offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb8,"Region*Year+Quarter"))


# year + quarter -landTemp
mod_nb9 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+year_quarter+commune+
                    offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb9,"No Temperature+Year-Quarter"))

# Land Temp Squared year +quarter
mod_nb10 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+I(landTemp^2)+year_quarter+commune+
                    offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb10,"T + T^2+Year-Quarter"))

# Temp spline
library(splines)
mod_nb11 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+ns(landTemp, df = 3)+year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb11,"Spline Temperature+Year-Quarter"))
df$landTemp %>% range()
attr(terms(mod_nb11),"predvars") # 16.5 & 27
lmtest::lrtest(model_nb, mod_nb11) # Spline is better is better


# save results
write.csv(models_nb_res,"Data/Models/modelSpecResults.csv",row.names = F)
models_nb_res <- read.csv("Data/Models/modelSpecResults.csv")

models_nb_res$name %>% unique()
cat_levels <- c("Time fixed effects",
                "Region interaction",
                "Temperature term")

order_models <- tibble(name=c(
  "Year*Month",
  "Year+Month",
  "Year*Quarter",
  "Year*Quarter+Month",
  "Year+Quarter",
  "Year+Quarter+Month",
  "Region*Quarter",
  "Region*Quarter+Year*Month",
  "Region*Quarter+Year+Month",
  "Region*Year+Quarter",
  "No Temperature+Year-Quarter",
  "T + T^2+Year-Quarter",
  "Spline Temperature+Year-Quarter"),
  category=c(rep(cat_levels[1],6),
             rep(cat_levels[2],4),
             rep(cat_levels[3],3)))
order_models$order_n <- 1:nrow(order_models)


# put title top
# https://stackoverflow.com/questions/70369558/two-column-facet-grid-with-strip-labels-on-top
library(gtable)
library(grid)
library(gridExtra)
f.TitleTop <- function(ggObject){
  
  gt <- ggplotGrob(ggObject)
  panels <-c(subset(gt$layout, grepl("panel", gt$layout$name), se=t:r))
  for(i in rev(panels$t-1)) {
    gt = gtable_add_rows(gt, unit(1.2, "lines"), i)
  }
  panels <-c(subset(gt$layout, grepl("panel", gt$layout$name), se=t:r))
  strips <- c(subset(gt$layout, grepl("strip-r", gt$layout$name), se=t:r))
  stripText = gtable_filter(gt, "strip-r")
  for(i in 1:length(strips$t)) {
    gt = gtable_add_grob(gt, stripText$grobs[[i]]$grobs[[1]], t=panels$t[i]-1, l=5)
  }
  gt = gt[,-6]
  # for(i in panels$t) { # remove for the all figure for the article, for some reason it interfer with the sec. axis
  #   gt$heights[i-1] = unit(1.2, "lines")
  #   gt$heights[i-2] = unit(0.2, "lines")
  # }
  return(gt)
}


# Figure 
p <- models_nb_res %>% 
  mutate(signif=sign(rr_low)==sign(rr_high)) %>% 
  left_join(order_models) %>% 
  mutate(category=factor(category,levels=cat_levels)) %>% 
  # name modification
  mutate(name=name %>% 
           str_replace_all("Year\\*Month","Year-Month") %>% 
           str_replace_all("Year\\*Quarter","Year-Quarter") %>% 
           str_replace("T \\+ T\\^2","T°+T° squared") %>% 
           str_replace_all("\\+"," + ")) %>% 
  # mutate(rowname=1:nrow(models_nb_res)) %>%
  filter(param=="pm25Exp_10ug") %>%
  # filter(param=="landTemp") %>%
ggplot(aes(reorder(name,order_n,decreasing=T),rr))+
  geom_linerange(aes(ymin=rr_low,ymax=rr_high))+
  geom_point(size=1,aes(col=signif))+
  # geom_point(size=1,col="red")+ # all T are significant
  # add separating lines
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=1)+
  # geom_vline(xintercept = c(3.5,7.5), linetype="dashed",col="grey",linewidth=0.3)+
  coord_flip()+
  # facet_wrap(~category,ncol=1,scales = "free_y")+
  facet_grid(category~.,scales = "free_y",space = "free")+
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  labs(title=expression(paste("Base Model: MR ~ ",PM[2.5],"+T°+Commune+...","")),
       x="Additional term",
       y=lab_rr)+
       # y=lab_rr_temp)+
  # Modify theme to look good
  theme_bw(12)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y=element_text(angle=0,vjust=1.025,margin=margin(r=-80)),
        axis.title.x = element_text(size=10,hjust=1),
        # strip.text = element_text(size = 8,angle=90),
        strip.text.y = element_text(angle = 0),
        legend.position = "none")



gt <- f.TitleTop(p)
grid.newpage();grid.draw(gt)

ggsave("Figures/Model/Model_Specifications.png",gt,
# ggsave("Figures/Model/Model_Specifications_Temp.png",gt,
       units="cm",dpi=500,
       # width = 1068/3.7795275591, # pixel to mm under dpi=300
       # height = 664/3.7795275591)
       width=8.7*2,height=8.7)


# Month coefficients
month_levels <- paste0("",12:2)
models_nb_res %>% 
  mutate(rowname=1:nrow(models_nb_res)) %>% 
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  filter(str_detect(param,"month"),!str_detect(param,":")) %>% 
  mutate(param=factor(str_remove(param,"month"),levels=month_levels)) %>% 
  ggplot(aes(param,rr))+
  geom_linerange(aes(ymin=rr_low,ymax=rr_high))+
  geom_point(size=1,aes(col=signif))+
  # add separating lines
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=0.4)+
  coord_flip()+
  facet_wrap(~name)+
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  labs(x="Month Fixed Effect",y=expression(paste("Percentage increase in Mortality w.r.t. Month 1")))+
  # Modify theme to look good
  theme_bw(12)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("Figures//Model/Model_Specifications_MonthEffect.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       # width = 1068/3.7795275591, # pixel to mm under dpi=300
       # height = 664/3.7795275591)
       width=8.7*2,height=8.7)


# # LM ----
model_lm <- glm(mortality ~ pm25Exp_10ug+year+region*quarter, 
                weights = pop75,
                data = df,
                family =  gaussian(link = "log"),
                na.action=na.omit)

summary(model_lm)
nobs(model_lm)
BIC(model_lm)
coef(model_lm) %>% exp()
confint(model_lm) %>% exp()
autoplot(model_lm)

## Gamma regression ----
model_gamma <- glm(mortality ~ pm25Exp_10ug+year+region*quarter, 
                   weights = pop75,
                   data = df,
                   family =  Gamma(link = "log"),
                   na.action=na.omit)

summary(model_gamma)
nobs(model_gamma)
BIC(model_gamma)
coef(model_gamma) %>% exp()
confint(model_gamma) %>% exp()
autoplot(model_gamma)


## Random Effects Region-----
model_random <- glmer.nb(Mortality_Count ~ pm25Exp_10ug+year+(1 | region)+
                           offset(log(pop75)), 
                         data = df,
                         na.action=na.omit)

summary(model_random)
nobs(model_random)
BIC(model_random)
fixef(model_random) %>% exp()
confint(model_random, method="Wald") %>% exp()

## Random Effects Comune-----
model_random_com <- glmer.nb(Mortality_Count ~ pm25Exp_10ug+year+(1 | commune)+
                               offset(log(pop75)), 
                             data = df,
                             na.action=na.omit)

summary(model_random_com)
nobs(model_random_com)
BIC(model_random_com)
fixef(model_random_com) %>% exp()
confint(model_random_com, method="Wald") %>% exp()



# EoF