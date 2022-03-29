#----clean----

#garde participant.es qui ont 1) fini 2 et 3) accepter les conditions
data_ambroise = dplyr::filter(data_ambroise_full, Finished == 1 & Q2.4_1 == 1 & Q2.3_2 == 1)

#change NA par 0 pour les FL_2*_DO_* variables
data_ambroise[136:138][is.na(data_ambroise[136:138])] <- 0

#double check que le script a bien fait son boulot
data_ambroise$checkA = ifelse(data_ambroise$FL_24_DO_AffectivePriming == data_ambroise$Priming, 1, 0)
data_ambroise$checkB = ifelse(data_ambroise$FL_24_DO_ComputationalPriming != data_ambroise$Priming, 1, 0)
data_ambroise$checkC = ifelse(data_ambroise$FL_27_DO_EvaluationA != data_ambroise$Product, 1, 0)
mean(data_ambroise$checkA) + mean(data_ambroise$checkB) + mean(data_ambroise$checkC) == 3


base  <- subset(data_ambroise, select= c("Priming",  "Reccomend", "Product", "Prix", "ProbabilitéACHAT", "EcologiqueEVAL", "Duration__in_seconds_", "id", "Q10.1", "Q12.1",  "Q12.2", "ComportementEnvironmental", "Q14.2", "Q14.3", "Q14.4", "Q14.5"))

colnames(base) <- c("Priming",  "Reccomend", "Product", "Price", "Buy", "Ecolo_Eval", "Time", "id", "Decision_mode", "negatif_affect",  "positive_affect", "Behav_enviro", "Age", "Gender", "Profession", "commentaires")


str(base)

#transformé age en continue et recategorisé Gender et profession
base$Age = as.numeric(as.character(base$Age))
base$positive_affect = as.numeric(as.character(base$positive_affect))
base$negatif_affect = as.numeric(as.character(base$negatif_affect))
base$Gender = as.factor(base$Gender); levels(base$Gender) <- c("Homme", "Femme", "Autre/NA", "Autre/NA")

base$Profession = as.factor(base$Profession); levels(base$Profession) <- c("Etudiant.e", "Actif.ve", "Les deux", "Les deux")

base$`Decision Mode` = as.factor(base$Decision_mode); levels(base$`Decision Mode`) <- c("Affectif", "Les deux", "Cognitif")



# base[c("Age", "Gender", "Profession", "Priming")] %>%  tbl_summary(by = Priming, statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} / {N} ({p}%)"),)  %>% add_stat_label() %>% add_p(pvalue_fun = ~style_pvalue(.x, digits = 2))  %>% modify_spanning_header(c("stat_1", "stat_2") ~ "**Priming Received**") %>% modify_caption("**Table 2. Participant Characteristics**") %>%
#   bold_labels()

#mettre en facteur Priming, Product et Decision mode
base$Priming = as.factor(base$Priming);  levels(base$Priming) <- c("Computational", "Affective") 
base$Product = as.factor(base$Product);  levels(base$Product) <- c("A+", "A++") 
base$Decision_mode = as.factor(base$Decision_mode)


#plot tout
base %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#gros OUTLIER dans time
with(base, Boxplot(Time, id=list(labels=base$id)))

# on enleve et on recommence
`%notin%` <- Negate(`%in%`)
base_clean = filter(base, id %notin% c("13288", "13570"))
with(base_clean, Boxplot(Time, id=list(labels=base_clean$id)))



#check correlation
base_clean %>%
  keep(is.numeric) %>%
  ggcorr(palette = "RdBu", label = TRUE)

#plot all
# base_clean %>%
#   keep(is.numeric) %>%
#   ggpairs()

plot(base_clean$Behav_enviro, base_clean$Age)

with(base_clean, Boxplot(Age, id=list(labels=base_clean$id)))

#straight lining ? par rapport a quel items?
base_clean$straight_lining = rowMeans(base_clean[c("Reccomend" ,     "Buy", "Ecolo_Eval",   "negatif_affect",   "positive_affect",   "Behav_enviro")])


x = transform(as.matrix(base_clean[c("Reccomend" ,     "Buy", "Ecolo_Eval",   "negatif_affect",   "positive_affect",   "Behav_enviro")]), SD=rowSds(as.matrix(base_clean[c("Reccomend" ,     "Buy", "Ecolo_Eval",   "negatif_affect",   "positive_affect",   "Behav_enviro")]), na.rm=TRUE)) # to check SD


#enlever sujet bad

base_clean = filter(base, id %notin% c("13288", "13570", "13006", "10930", "12811", "10945","13627"))

# Score of Affect towards climate change
base_clean$score = -base_clean$negatif_affect
base_clean$score = base_clean$score+ base_clean$positive_affect

