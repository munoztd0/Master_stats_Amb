
### Setup {-}
#this is to check to see if package are installed and if not to install them or just load them if they are installed!
if(!require(pacman)) {install.packages("pacman")}

pacman::p_load(crsh/papaja@devel, tinylabels, apaTables, haven, ggplot2,  tidyverse, gtsummary, car, GGally, MASS, matrixStats, rcompanion, moments, utils, sjPlot, interactions, kableExtra)


#get relative path
path =  here("Ambroise") #this is really important ! you juste need to put the name of YOUR folder and here() will find the relative path for you ! 
#or path =  dirname(rstudioapi::getActiveDocumentContext()$path) in R
setwd(path) #set working directory

# load data
base_clean <- read_csv("data.csv")

base[c("Age", "Gender", "Profession")] %>%  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} / {N} ({p}%)"),)  %>% modify_caption("**Table 1. Demographics **") %>%
  bold_labels()

### Statistics

#####  Correlation 

base_clean$Gender = recode_factor(base_clean$Gender, "Autre/NA" = NA_character_)

ggpubr::ggscatter(base_clean, x = "negatif_cilmat", y = "positive_climate", position = position_jitter(w = 0.1, h = 0.1),
                  color = "black", shape = 21, size = 3, # Points color, shape and size
                  add = "reg.line",  # Add regressin line
                  add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                  conf.int = TRUE, # Add confidence interval
                  cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                  cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n")
)


#### Price
base_price = base_clean #no huge outliers, see appendix

final_price = lm(Price ~ Priming*Product+ Behav_enviro + Age,  data = base_price)

apa_lm <- apa_print(anova(final_price))
apa_table(apa_lm$table,  caption = "Anova table for Price.")


plot_model(final_price)

sjPlot::plot_model(final_price, type = "pred")[3]



#### Probability to buy

base_buy = filter(base_clean, id %notin% c("13804")) #remove huge outliers, see appendix


final_buy = lm(Buy ~ Priming*Product + Behav_enviro + Age,  data = base_buy)

apa_lm <- apa_print(anova(final_buy))
apa_table(apa_lm$table,  caption = "Anova table for Probability to buy.")

sjPlot::plot_model(final_buy)


sjPlot::plot_model(final_buy, type = "pred")[2]
sjPlot::plot_model(final_buy, type = "pred")[3]
sjPlot::plot_model(final_buy, type = "pred")[4]


  
#### Reccomendation

base_recco = base_clean #no huge outliers, see appendix

final_recco = lm(Reccomend ~ Priming*Product+Product*Behav_enviro + Age + Gender,  data = base_recco)

apa_lm <- apa_print(anova(final_recco))
apa_table(apa_lm$table,  caption = "Anova table for Reccomendation.")

sjPlot::plot_model(final_recco)

sjPlot::plot_model(final_recco, type = "pred")[2]
sjPlot::plot_model(final_recco, type = "pred")[3]

interactions::interact_plot(final_recco, pred = Behav_enviro, modx = Product, interval = TRUE)



  
#### Ecologic Evaluation


base_eco = filter(base_clean, id %notin% c("13594", "11200", "13804", "12529", "11017")) #remove huge outliers, see appendix

final_eco = lm(Ecolo_Eval ~ Priming*Product+Behav_enviro + Age + Gender,  data = base_eco)

apa_lm <- apa_print(anova(final_eco))
apa_table(apa_lm$table, caption = "Anova table for Reccomendation.")

sjPlot::plot_model(final_eco)

sjPlot::plot_model(final_eco, type = "pred")[2]
sjPlot::plot_model(final_eco, type = "pred")[3]


  
### Appendix
  

#### Visual check normality
  

plotNormalHistogram(base_clean$Price, main = "Price", sub = paste("skewness =", skewness(base_clean$Price, na.rm = TRUE))) 

plotNormalHistogram(base_clean$Reccomend, main = "Reccomendation", sub = paste("skewness =", round(skewness(base_clean$Reccomend, na.rm = TRUE)),2))


plotNormalHistogram(base_clean$Buy, main = "Probability to buy", sub = 
                      paste("skewness =", round(skewness(base_clean$Buy, na.rm = TRUE),2)))


plotNormalHistogram(base_clean$Ecolo_Eval, main = "Ecologic Evaluation", sub =
                      paste("skewness =", round(skewness(base_clean$Ecolo_Eval, na.rm = TRUE),2)))


  
  
#### Model selection for price

modprice = lm(Price ~ Priming*Product*Behav_enviro + Priming*Product*Decision_mode + Priming*Product*Age, data = base_clean)

MS = MASS::stepAIC(modprice, direction = "both", trace = FALSE)

x = kable(attributes(MS$anova)$heading, format="html") %>%
  kable_styling(latex_options = "HOLD_position", position = "center", full_width = F) %>%  row_spec(1, bold=T,align='c') 
gsub("<thead>.*</thead>", "", x)


kable(MS$anova, format="html") %>%
  kable_styling(latex_options = "HOLD_position", position = "center", full_width = F) %>%  row_spec(0, bold=T,align='c')

#### Price: Diagnostics Plots for linear model 
  

modprice = lm(Price ~ Priming + Product + Behav_enviro + Age + Priming:Product, data = base_price)

plot(modprice, c(1:2,4), labels.id = base_price$id)
sjPlot::plot_model(modprice, type="diag")[[3]]


#### Model selection for Probality to buy


modbuy = lm(Buy ~ Priming*Product*Behav_enviro + Priming*Product*Decision_mode + Priming*Product*Age, data = base_clean)

MS = MASS::stepAIC(modbuy, direction = "both", trace = FALSE)

x = kable(attributes(MS$anova)$heading, format="html") %>%
  kable_styling(latex_options = "HOLD_position", position = "center", full_width = F) %>%  row_spec(1, bold=T,align='c') 
gsub("<thead>.*</thead>", "", x)


kable(MS$anova, format="html") %>%
  kable_styling(latex_options = "HOLD_position", position = "center", full_width = F) %>%  row_spec(0, bold=T,align='c')


  
  
#### Probability to buy: Diagnostics Plots for linear model  

modbuy = lm(Buy ~ Product*Priming + Behav_enviro + Age + Product:Behav_enviro, data = base_buy)

plot(modbuy, c(1:2,4), labels.id = base_buy$id)
sjPlot::plot_model(modbuy, type="diag")[[3]]

  
#### Model selection for Reccomendation

modrecco = lm(Reccomend ~ Priming*Product*Behav_enviro + Priming*Product*Decision_mode + Priming*Product*Age, data = base_clean)

MS = MASS::stepAIC(modrecco, direction = "both", trace = FALSE)

x = kable(attributes(MS$anova)$heading, format="html") %>%
  kable_styling(latex_options = "HOLD_position", position = "center", full_width = F) %>%  row_spec(1, bold=T,align='c') 
gsub("<thead>.*</thead>", "", x)


kable(MS$anova, format="html") %>%
  kable_styling(latex_options = "HOLD_position", position = "center", full_width = F) %>%  row_spec(0, bold=T,align='c')



#### Reccomendation: Diagnostics Plots for linear model AFTER

modrecco = lm(Reccomend ~ Priming*Product + Behav_enviro + Product:Behav_enviro, data = base_recco)

plot(modrecco, c(1:2,4), labels.id = base_recco$id)
sjPlot::plot_model(modrecco, type="diag")[[3]]

  
  
#### Model selection for Ecologic Evaluation

modeco = lm(Ecolo_Eval ~ Priming*Product*Behav_enviro + Priming*Product*Decision_mode + Priming*Product*Age, data = base_clean)

MS = MASS::stepAIC(modeco, direction = "both", trace = FALSE)

x = kable(attributes(MS$anova)$heading, format="html") %>%
  kable_styling(latex_options = "HOLD_position", position = "center", full_width = F) %>%  row_spec(1, bold=T,align='c') 
gsub("<thead>.*</thead>", "", x)


kable(MS$anova, format="html") %>%
  kable_styling(latex_options = "HOLD_position", position = "center", full_width = F) %>%  row_spec(0, bold=T,align='c')



#### Ecologic Evaluation: Diagnostics Plots for linear model AFTER

modeco = lm(Ecolo_Eval ~ Priming*Product*Decision_mode + Behav_enviro + Priming*Product*Age, data = base_eco)

plot(modeco, c(1:2,4), labels.id = base_eco$id)
sjPlot::plot_model(modeco, type="diag")[[3]]

  
  
  
report::report_packages()


