library(readr)
library(tidyverse)
library(mice)

df = read_csv('/Users/georgeberry/Desktop/CAH/cah_data_files/201710-CAH_PulseOfTheNation_Raw.csv')

colnames(df) = c('income',
                 'gender',
                 'age',
                 'age_range',
                 'pol_affil',
                 'approve',
                 'educ',
                 'blank1',
                 'race',
                 'blank2',
                 'nationalist',
                 'rep_nationalist',
                 'blank3',
                 'love_amer',
                 'dem_love_amer',
                 'blank4',
                 'help_poor',
                 'rep_help_poor',
                 'white_racist',
                 'dem_white_racist',
                 'lost_friends',
                 'civil_war',
                 'hunting',
                 'kale',
                 'the_rock',
                 'vader_or_trump')

df = df %>%
  mutate_if(is.character, as.factor)

df %>%
  select(age, age_range, gender, income, race, educ, pol_affil, civil_war, lost_friends) %>%
  mutate_all(funs(ifelse(. == 'DK/REF', NA, .))) %>%
  summarize_all(funs(sum(is.na(.))))

cut_down_df = df %>%
  select(age, age_range, income, gender, race, educ, pol_affil, civil_war, lost_friends) %>%
  mutate_if(is.factor, funs(factor(replace(., . == 'DK/REF', NA)))) %>%
  .[complete.cases(.),] %>%
  mutate(civil_war = ifelse(civil_war == 'Likely', 1, 0),
         race = relevel(race, ref=5),
         # recode this or else we get HUGE ses for Asian
         race = factor(replace(race, race %in% c('Asian', 'Latino'), 'Other')),
         gender = relevel(gender, ref=2),
         educ = relevel(educ, ref=3),
         pol_affil = relevel(pol_affil, ref=1))  

clean_df = df %>%
  select(age, age_range, gender, race, educ, pol_affil, civil_war, lost_friends) %>%
  mutate_if(is.factor, funs(factor(replace(., . == 'DK/REF', NA)))) %>%
  .[complete.cases(.),] %>%
  mutate(civil_war = ifelse(civil_war == 'Likely', 1, 0),
         race = relevel(race, ref=5),
         gender = relevel(gender, ref=2),
         educ = relevel(educ, ref=3),
         pol_affil = relevel(pol_affil, ref=1))

prep_df = df %>%
  select(age, age_range, income, gender, race, educ, pol_affil, civil_war, lost_friends) %>%
  mutate_if(is.factor, funs(factor(replace(., . == 'DK/REF', NA)))) %>%
  mutate(civil_war = ifelse(civil_war == 'Likely', 1, 0),
         race = relevel(race, ref=5),
         gender = relevel(gender, ref=2),
         educ = relevel(educ, ref=3),
         pol_affil = relevel(pol_affil, ref=1))

tmp_df = mice(prep_df, m=5, maxit=50, meth='pmm', seed=500)
imputed_df = complete(tmp_df, 1)
attr(imputed_df$race, 'contrasts') = NULL
attr(imputed_df$gender, 'contrasts') = NULL
attr(imputed_df$educ, 'contrasts') = NULL
attr(imputed_df$pol_affil, 'contrasts') = NULL
attr(imputed_df$lost_friends, 'contrasts') = NULL

mod_cut_down = glm(civil_war ~ race + age + log(income) + I(age^2) + gender + educ + pol_affil + lost_friends, family=binomial, data=cut_down_df)
summary(mod_cut_down)

mod_clean = glm(civil_war ~ race + age + I(age^2) + gender + educ + pol_affil + lost_friends, family=binomial, data=clean_df)
summary(mod_clean)

mod_imputed = glm(civil_war ~ race + age + log(income) + I(age^2) + gender + educ + pol_affil + lost_friends, family=binomial, data=imputed_df)
summary(mod_imputed)

result_df_cut_down = data.frame(
  var_name = labels(mod_cut_down$coefficients),
  means = mod_cut_down$coefficients,
  ses = sqrt(diag(vcov(mod_cut_down))),
  mod_type='cut_down'
)

result_df_clean = data.frame(
  var_name = labels(mod_clean$coefficients),
  means = mod_clean$coefficients,
  ses = sqrt(diag(vcov(mod_clean))),
  mod_type='clean'
)

result_df_imputed = data.frame(
  var_name = labels(mod_imputed$coefficients),
  means = mod_imputed$coefficients,
  ses = sqrt(diag(vcov(mod_imputed))),
  mod_type='imputed'
)

result_df = rbind(result_df_clean, result_df_imputed, result_df_cut_down) %>%
  mutate(ucl = means + 1.96 * ses, lcl = means - 1.96 * ses)

dodge <- position_dodge(width=0.5)
ggplot(result_df) +
  geom_point(aes(x=var_name, y=means, color=mod_type), position=dodge) +
  geom_errorbar(aes(ymax = ucl, ymin = lcl, x=var_name, color=mod_type), position=dodge) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# predict

# white, income

new_cah.df.a <- data.frame(income=0:250000,
                         lost_friends='No',
                         age = 46,
                         gender = "Male",
                         pol_affil = "Independent",
                         educ = "College degree",
                         race = "White")
new_cah.df.b <- data.frame(income=0:250000,
                           lost_friends='Yes',
                           age = 46,
                           gender = "Male",
                           pol_affil = "Independent",
                           educ = "College degree",
                           race = "White")
new_cah.df.c <- data.frame(income=0:250000,
                           lost_friends='No',
                           age = 46,
                           gender = "Female",
                           pol_affil = "Independent",
                           educ = "College degree",
                           race = "White")
new_cah.df.d <- data.frame(income=0:250000,
                           lost_friends='Yes',
                           age = 46,
                           gender = "Female",
                           pol_affil = "Independent",
                           educ = "College degree",
                           race = "White")
new_cah.df <- rbind(new_cah.df.a, new_cah.df.b, new_cah.df.c, new_cah.df.d)

new_cah.df$prediction <- predict(mod_imputed, new_cah.df, type='response')

predmodel1 <- ggplot(new_cah.df, aes(x = income, y = prediction, linetype = as.factor(lost_friends),
                       color = as.factor(gender))) + 
  geom_smooth() + ggtitle('Prediction for Likely/Unlikely Civil War \nGiven Lost Friend (White, College Degree)') + 
  xlab('Income') + ylab('Predicted Probability') + labs(linetype='Lost Friends?', color='Gender') + ylim(0,1)

# black, income

new_cah.df.a <- data.frame(income=0:250000,
                           lost_friends='No',
                           age = 46,
                           gender = "Male",
                           pol_affil = "Independent",
                           educ = "High school",
                           race = "Black")
new_cah.df.b <- data.frame(income=0:250000,
                           lost_friends='Yes',
                           age = 46,
                           gender = "Male",
                           pol_affil = "Independent",
                           educ = "High school",
                           race = "Black")
new_cah.df.c <- data.frame(income=0:250000,
                           lost_friends='No',
                           age = 46,
                           gender = "Female",
                           pol_affil = "Independent",
                           educ = "High school",
                           race = "Black")
new_cah.df.d <- data.frame(income=0:250000,
                           lost_friends='Yes',
                           age = 46,
                           gender = "Female",
                           pol_affil = "Independent",
                           educ = "High school",
                           race = "Black")
new_cah.df <- rbind(new_cah.df.a, new_cah.df.b, new_cah.df.c, new_cah.df.d)

new_cah.df$prediction <- predict(mod_imputed, new_cah.df, type='response')

predmodel2 <- ggplot(new_cah.df, aes(x = income, y = prediction, linetype = as.factor(lost_friends),
                                     color = as.factor(gender))) + 
  geom_smooth() + ggtitle('Prediction for Likely/Unlikely Civil War \nGiven Lost Friend (Black, High School)') + 
  xlab('Income') + ylab('Predicted Probability') + labs(linetype='Lost Friends?', color='Gender') + ylim(0,1)


#age test

new_cah.df.a <- data.frame(income=55000,
                           lost_friends='No',
                           age = 18:85,
                           gender = "Male",
                           pol_affil = "Independent",
                           educ = "High school",
                           race = "Black")
new_cah.df.b <- data.frame(income=55000,
                           lost_friends='Yes',
                           age = 18:85,
                           gender = "Male",
                           pol_affil = "Independent",
                           educ = "High school",
                           race = "Black")
new_cah.df.c <- data.frame(income=55000,
                           lost_friends='No',
                           age = 18:85,
                           gender = "Female",
                           pol_affil = "Independent",
                           educ = "High school",
                           race = "Black")
new_cah.df.d <- data.frame(income=55000,
                           lost_friends='Yes',
                           age = 18:85,
                           gender = "Female",
                           pol_affil = "Independent",
                           educ = "High school",
                           race = 'Black')
new_cah.df <- rbind(new_cah.df.a, new_cah.df.b, new_cah.df.c, new_cah.df.d)

new_cah.df$prediction <- predict(mod_imputed, new_cah.df, type='response')

predmodel3 <- ggplot(new_cah.df, aes(x = age, y = prediction, linetype = as.factor(lost_friends),
                       color = as.factor(gender))) + 
  geom_smooth() + ggtitle('Prediction for Likely/Unlikely Civil War \nGiven Lost Friend (Black, High School)') + 
  xlab('Age') + ylab('Predicted Probability') + labs(linetype='Lost Friends?', color = 'Gender') + ylim(0,1)



# =============================
# tables for reporting
# =============================

library(stargazer)

# ==================================
# ORGANIZE REGRESSION TABLES

coef.mod_cut_down <- exp(mod_cut_down$coef)
pval.mod_cut_down <- summary(mod_cut_down)$coef[, 4]
coef.mod_clean <- exp(mod_clean$coef)
pval.mod_clean <- summary(mod_clean)$coef[, 4]
coef.mod_imputed <- exp(mod_imputed$coef)
pval.mod_imputed <- summary(mod_imputed)$coef[, 4]


library(stargazer)
#stargazer(model.count,coef=list(coef.vector))

stargazer(mod_cut_down,
          mod_clean,
          mod_imputed, 
          coef=list(coef.mod_cut_down,
                    coef.mod_clean,
                    coef.mod_imputed),
          p=list(pval.mod_cut_down,
                 pval.mod_clean,
                 pval.mod_imputed),
          type='html',
          column.labels=c("No-Missing-Data Model",
                          "No-Income Model",
                          "Imputed Model"),
          no.space=TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001)#,
          #report=('vc*') #          report=('vc*p')
)
