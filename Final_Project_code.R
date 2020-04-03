library(dplyr)
library(lme4)
library(lmerTest)
library(tidyverse)
options(scipen = 999)
#Preprocessing
PSYC746_final = trt
#rename columns####
colnames(PSYC746_final)[colnames(PSYC746_final)=="seq"] <- "Trial_order"
colnames(PSYC746_final)[colnames(PSYC746_final)=="subj"] <- "Subject"
colnames(PSYC746_final)[colnames(PSYC746_final)=="item"] <- "ITEM"
colnames(PSYC746_final)[colnames(PSYC746_final)=="cond"] <- "Condition_nb"
colnames(PSYC746_final)[colnames(PSYC746_final)=="R1"] <- "TRT_beginning"
colnames(PSYC746_final)[colnames(PSYC746_final)=="R2"] <- "TRT_modifier"
colnames(PSYC746_final)[colnames(PSYC746_final)=="R3"] <- "TRT_target"
colnames(PSYC746_final)[colnames(PSYC746_final)=="R4"] <- "TRT_end"

#Create language group column####
PSYC746_final$Group = ifelse(PSYC746_final$Subject <14, "English_L1", "French_L1")

#Create condition column####
PSYC746_final$Condition = ifelse(PSYC746_final$Condition_nb == 1, "Modifier-dominant", 
                                 (ifelse(PSYC746_final$Condition_nb == 2, "Modifier-subordinate", 
                                         (ifelse(PSYC746_final$Condition_nb == 3, "Bare-dominant", 
                                                 (ifelse(PSYC746_final$Condition_nb == 4, "Bare-subordinate", "DELETE")))))))

#remove filler trials####
PSYC746_final = subset(PSYC746_final, Condition != "DELETE")

#create context column####
PSYC746_final$Context = ifelse(PSYC746_final$Condition_nb == 1, "Modifier", 
                               (ifelse(PSYC746_final$Condition_nb == 2, "Modifier", "Bare")))

#create word type column####
PSYC746_final$WordType = ifelse(PSYC746_final$ITEM <33, "Cognate Homonym", "English Homonym")

#create dominance column####
PSYC746_final$Dominance = ifelse(PSYC746_final$Condition_nb == 1, "Dominant", 
                                 (ifelse(PSYC746_final$Condition_nb == 2, "Subordinate", 
                                         (ifelse(PSYC746_final$Condition_nb == 3, "Dominant", "Subordinate")))))

#remove target skipped####
summary(PSYC746_final$TRT_target)
which(is.na(PSYC746_final$TRT_target))
#25 skips
25*100/2013
PSYC746_final = PSYC746_final %>% filter(!is.na(TRT_target))
#we remove 1.24% of the data

hist(PSYC746_final$TRT_target)
PSYC746_final$log_TRT_target = log(PSYC746_final$TRT_target)
hist(PSYC746_final$log_TRT_target)

PSYC746_final$Group_dev =ifelse(PSYC746_final$Group == "English_L1", -0.5, 0.5)
PSYC746_final$WordType_dev =ifelse(PSYC746_final$WordType == "English Homonym", -0.5, 0.5)
PSYC746_final$Dominance_dev =ifelse(PSYC746_final$Dominance == "Dominant", -0.5, 0.5)
PSYC746_final$Context_dev =ifelse(PSYC746_final$Context == "Bare", -0.5, 0.5)

#null model####
Null = lmer(log_TRT_target ~ 1+(1|Subject) + (1|ITEM), data=PSYC746_final, REML = T)
summary(Null)
logLik(Null)
confint(Null) #nothing includes 0

qqnorm(residuals(Null)) #looks pretty ok!
qqline(residuals(Null))

#ICC calculation
0.02596 / (0.02596 + 0.27660)
#.086 = 8.6% of the variance is due to variability between items 
#(correlation between 2 trials from the same item, but from 2 different participants?)
0.20575 / (0.20575 + 0.27660)
#.427 = 42.7% of the variance is due to variability between subjects
#(correlation between 2 trials from the same participant, but from 2 different items?)

#according to Carson & Beeson (2013, see also  Locker et al., 2007), 
#one should calculate the ICC in that cross-classified data structure 
#by dividing the random effect (by participant or by items) by the total variance (by participant +
#by item + residuals)
(0.02596)/(0.02596+0.20575+0.27660)
#ICC by item = 5.11% total variance
(0.20575)/(0.20575+0.02596+0.27660)
#ICC by participant = 40.48% total variance
(0.27660)/(0.27660+0.20575+0.02596)
#54.416% of the variance is unnaccounted for

#DEFF
#by participants
x = as.data.frame(table(PSYC746_final$Subject))
mean(x$Freq)
#mean number of observations (L1) per participant (L2) = 62.125
deff1 = 1+0.4047727*(62.125-1)
#25.742
#effective sample size
32/25.742
#1.243 (!???)
#DEFT = 1.115 (square root of DEFF) > SE are 1.115 times larger than estimated

#by items
x = as.data.frame(table(PSYC746_final$ITEM))
mean(x$Freq)
#mean number of observations (L1) per item (L2) = 31.0625
deff2 = 1+0.0510712*(31.0625-1)
#2.535
#effective number of items
64/2.535
#25.247 (???)
#DEFT = 5.025 (square root of DEFF) > SE are 5.025 times larger than estimated

#model with group predictor####
Model2 = lmer(log_TRT_target ~ Group_dev +
                (1|Subject) + 
                (1|ITEM), data=PSYC746_final, REML = T)
summary(Model2)
logLik(Model2) #log likelihood of null model is lower, so better
confint(Model2) #group_dev includes 0...

#deviance (should I do this if I am comparing only fixed effects?)
3298.2-3296.8
#1.4 (df = 1)
#The P-Value is .236724. The result is not significant at p < .05.

anova(Null, Model2) #not sure if I should divide the p value by 2, as I only have 1 predictor > we should not, because
#we are comparing models differing by a fixed, not a random effect...

#Difference is residuals
tau2change_p = 0.20575-0.19112
#.015
tau2change_i = 0.02596-0.02596
#0
sigma2change = 0.27660-0.27660
#0

#variance reduced
L1_var_reduction <- sigma2change / 0.27660
#0
L2_var_reduction_p <- tau2change_p/0.20575
#0.071
L2_var_reduction_i <- tau2change_i/0.02596
#0

#condition ICC
0.19112/ (0.19112 + 0.27660)
#.409 = 40.9% of the variance is due to variability between subjects
0.02596 / (0.02596 + 0.27660)
#.086 = 8.6% of the variance is still due to variability between items (no change)

#95% plausible values range for intercepts
#how much do means vary across participants?
SDx2 <- (2*(sqrt(0.19112)))
Upper_range <- 6.1934 + SDx2
#7.068
lower_range <- 6.1934 - SDx2
#5.319

#95% plausible values range for intercepts
#how much do means vary across items?
SDx2 <- (2*(sqrt(0.02596)))
Upper_range <- 6.1934 + SDx2
#6.516
lower_range <- 6.1934 - SDx2
#5.871

#L1 residuals
l1_residuals <- tibble::enframe(residuals(Model2))
PSYC746_final <- PSYC746_final %>% 
  bind_cols(l1_residuals) %>% 
  select(-name) %>% 
  rename(l1resid = value)

PSYC746_final %>% ggplot(mapping = aes(x = l1resid)) +
  geom_histogram()


#L2 residuals -----------------------------------------------

par(mfrow=c(1,2))

PSYC746_finalByParticRanef <- ranef(Model2)$Subject[['(Intercept)']]
qqnorm(PSYC746_finalByParticRanef,  main = "participant means") #2 weirdos
qqline(PSYC746_finalByParticRanef)

PSYC746_finalByItemRanef <- ranef(Model2)$ITEM[['(Intercept)']]
qqnorm(PSYC746_finalByItemRanef,  main = "item means") #1 weirdo
qqline(PSYC746_finalByItemRanef)

summary(PSYC746_finalByParticRanef)
#mean of 0, which is great, but median is different, so there is a hint of nonnormality
summary(PSYC746_finalByItemRanef)
#mean of 0, which is great, but median is different, so there is a hint of nonnormality

l2_residuals_p <- ranef(Model2)$Subject
l2_residuals_p %>% 
  ggplot(mapping = aes(x = `(Intercept)`)) +
  geom_histogram()
#ARK NOPE

l2_residuals_i <- ranef(Model2)$ITEM
l2_residuals_i %>% 
  ggplot(mapping = aes(x = `(Intercept)`)) +
  geom_histogram()
#ARK NOPE

#ouliers
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

temp_int <- l2_residuals_p %>% 
  tibble::rownames_to_column(var="outlier") %>% 
  mutate(is_outlier = ifelse(is_outlier(`(Intercept)`), `(Intercept)`, as.numeric(NA)))
temp_int$outlier[which(is.na(temp_int$is_outlier))] <- as.numeric(NA)

ggplot(temp_int, aes(y = `(Intercept)`, x = 0)) +
  geom_boxplot()  +
  geom_text(aes(label = outlier), na.rm = TRUE, nudge_y = 0.2)

#Participant 29 and 8 are outliers!

temp_int <- l2_residuals_i %>% 
  tibble::rownames_to_column(var="outlier") %>% 
  mutate(is_outlier = ifelse(is_outlier(`(Intercept)`), `(Intercept)`, as.numeric(NA)))
temp_int$outlier[which(is.na(temp_int$is_outlier))] <- as.numeric(NA)

ggplot(temp_int, aes(y = `(Intercept)`, x = 0)) +
  geom_boxplot()  +
  geom_text(aes(label = outlier), na.rm = TRUE, nudge_y = 0.2)

#Item 2 is an outlier!

# Relatedness-Merge l2_predictors and l2_residuals --------------------------------

l2_residuals_rowname <- rownames_to_column(l2_residuals_p) %>% 
  rename("Subject" = rowname) %>%
  mutate("Subject" = as.integer(Subject)) %>% 
  rename(intercept_residual = `(Intercept)`) %>% # rename syntax: rename(new_name = old_name)
  as_tibble()

PSYC746_final <- full_join(PSYC746_final, l2_residuals_rowname) 

PSYC746_final %>%
  ggplot() + 
  geom_point(mapping = aes(x = l1resid, y = intercept_residual))
#does not look related!

#Are L1 residuals related to L2 predictor group_dev? 

PSYC746_final %>% ggplot(mapping = aes(x = Group_dev, y = l1resid)) +
  geom_point() +
  labs(x = "L2 predictor: Native language", y = "L1 residuals")
#does not look related!
cor.test(PSYC746_final$Group_dev, PSYC746_final$l1resid)
#-0.000 (t = -0.000, p = 1)
#there is no relationship between L2 predictor (group) and L2 intercept residuals

#Are L2 intercept residuals independent from L2 predictor group_dev? ----------
PSYC746_final %>% 
  ggplot(mapping = aes(x = Group_dev, y = intercept_residual)) +
  geom_point() +
  labs(x = "L2 predictor: Native language", y = "Intercept Residuals")

cor.test(PSYC746_final$Group_dev, PSYC746_final$intercept_residual)
#0.007 (t = 0.289, p = .772)
#there is no relationship between L2 predictor (group) and L2 intercept residuals






#model with context predictor####
Model3 = lmer(log_TRT_target ~ Group_dev*Context_dev+
                (1+Context_dev||Subject) + 
                (1+Group_dev||ITEM), data=PSYC746_final, REML = T)
summary(Model3)





#Descriptive stuff####
lattice::densityplot(PSYC746_final$TRT_target)
lattice::densityplot(PSYC746_final$log_TRT_target)

PSYC746_final %>% group_by(Group) %>%
  summarise(mean=mean(TRT_target, na.rm = T))
#English L1 mean TRT = 558ms
#French L1 mean TRT = 742ms
#looks ok

PSYC746_final %>% group_by(Group, WordType) %>%
  summarise(mean=mean(TRT_target, na.rm = T))
#English L1 cognate homonym 577
#English L1 english homonym 539
#French L1 cognate homonym 748
#French L1 English homonym 735

PSYC746_final %>% group_by(Group, WordType, Dominance) %>%
  summarise(mean=mean(TRT_target, na.rm = T))
#English L1 cognate homonym dominant 550
#English L1 cognate homonym subordinate 605
#English L1 english homonym dominant 481
#English L1 english homonym subordinate 597
#French L1 cognate homonym dominant 689
#French L1 cognate homonym subordinate 806
#French L1 english homonym dominant 628
#French L1 english homonym subordinate 843

PSYC746_final %>% group_by(Group, Context, WordType, Dominance) %>%
  summarise(mean=mean(TRT_target, na.rm = T))

#519-471 #48 SBE English L1 coghom
#724-699 #25 SBE French L1 coghom > reduced!
#486-409 #77 SBE English L1 enghom
#740-581 #159 SBE French L1 enghom > boosted!

#Pattern looks promising!
