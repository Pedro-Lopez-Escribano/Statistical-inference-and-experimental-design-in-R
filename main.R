library(tidyverse)

#---------------------------------


#------------------------------------------
 #Data 2

df <- data.frame(
    promote = c(rep("promoted", 350), rep("not_promoted", 130)),
    sex = c(rep("male", 180), rep("female",170), # promoted
            rep("male", 50), rep("female",80)))    # not_promoted

# Fewer women were promoted

df %>% 
    count(sex, promote)

df %>% 
    group_by(sex) %>%
    summarize(promote_prop = mean(promote == "promoted"))


# The important stadistical question to ask after looking at the data is as follows : 
# is it plausible to observe such a difference in proportions,
# in a scenario where men and women are equally to be promoted?

# GENDER DISCRIMATION HYPOTHESES

# H0: gender and promotion are unrelated variables.
# H1: men are more likely to be promoted


# Randomizing gender discrimination 

diff_orig <- df %>% 
    group_by(sex) %>%
    summarize(prop_prom = mean(promote == "promoted")) %>%
    summarize(stat = diff(prop_prom)) %>% 
    pull()


library(infer)

df_perm <- df %>%
    specify(promote ~ sex, success = "promoted") %>% 
    hypothesize(null = "independence") %>% 
    generate(reps = 1000, type = "permute") %>% 
    calculate(stat = "diff in props", order = c("male","female"))

ggplot(df_perm, aes(x = stat)) + 
    geom_histogram(binwidth = 0.03) + 
    geom_vline(xintercept = diff_orig, color = "red")

df_perm %>%
    visualize() +
        shade_pvalue(obs_stat = diff_orig, direction = "greater")

df_perm %>% 
    get_p_value(obs_stat = diff_orig, direction = "greater")

# In the population there is evidence that women are promote at diferent rate, 
# but we cannot tell whether the difference is due to discrimination or something else.
#----------------------------------------------------

# bootstrap
# Data 3 : one column with polls id, and one column with yes or no, that data represent votes for one candidate in a election. 


all_polls <- readRDS("all_polls.rds")

# Resampling from a sample

# P-hats from all polls
exp1_props <- all_polls %>% 
    group_by(poll) %>% 
    summarize(stat = mean(vote == "yes"), n = n())

# P-hats from one poll with bootstrap
exp2_props <- all_polls %>% 
    filter(poll == 1) %>% 
    specify(response = vote, success = "yes") %>% 
    generate(reps = 1000, type = "bootstrap") %>% 
    calculate(stat = "prop")

exp1_props %>% summarize(variability = sd(stat))

exp2_props %>% summarize(variability = sd(stat))
    
    
combined_exp <- bind_rows(exp1_props, exp2_props, .id = "experiment")    

ggplot(combined_exp, aes(stat, color = experiment)) + 
    geom_density(bw =0.1)
    
#--------------------------------------

# Bootstrap t-confindence interval

one_poll <- all_polls %>% 
    filter(poll == 1) %>% 
    select(vote)

one_poll_bootstrap <- one_poll %>% 
    specify(response = vote , success = "yes") %>% 
    generate(reps = 1000, type = "bootstrap") %>% 
    calculate(stat = "prop")

p_hat <- one_poll %>% 
    summarize(stat = mean(vote == "yes")) %>% pull() 

ci <- one_poll_bootstrap %>% 
    summarize(lower = p_hat - (2* sd(stat)),
              upper = p_hat + (2* sd(stat)))
ci

one_poll_bootstrap %>% 
    visualise() + 
        shade_ci(ci)


#--------------------------------------------------------------------------------
#------------------------------------------------
# Note about Power an sample size: 

# Power: probability that the test correctly rejects the null hypothesis,
# when the alternative hypothesis is true

# Effect size : standardized measure of the difference yo're trying to detect

# Sample size : how many experimental units you need to survey to detect the desired
# difference at the desired power
#------------------------

library(pwr)

pwr.anova.test(k = 3, # groups
               n = 20, # N
               f = 0.2, # effect size  
               sig.level = 0.05,
               power =  NULL)  # arg to calculate

# In this case power is low, so probaly we can't detect
# a effect size so small with that number of people per group
#-----------------------
#  We want to know what N needs to detect size effect of 0.25, 
# with sig.level = 0.05, from one sample, alternative hypothesy "greater" and power 80 %

pwr.t.test(n = NULL, 
           d = 0.25,
           sig.level = 0.05,
           type = "one.sample", alternative = "greater",
           power = 0.8)

#-------------------------------------------------

#Data

lendingclub <- read.csv("https://assets.datacamp.com/production/repositories/1793/datasets/e14dbe91a0840393e86e4fb9a7ec1b958842ae39/lendclub.csv",
               stringsAsFactors = T)
lendingclub <- as_tibble(lendingclub)
str(lendingclub)
glimpse(lendingclub)

# Some EDA

lendingclub %>% 
    summarize(
        median_loan_amnt = median(loan_amnt),
        mean_int_rate = mean(int_rate),
        mean_annual_inc = mean(annual_inc)
    )

ggplot(lendingclub, aes(x = purpose)) + 
    geom_bar() + coord_flip()

# Recode purpose var

lendingclub$purpose_recode <- lendingclub$purpose %>% recode( 
    "credit_card" = "debt_related", 
    "debt_consolidation" = "debt_related",
    "medical" = "debt_related",
    "car" = "big_purchase", 
    "major_purchase" = "big_purchase", 
    "vacation" = "big_purchase",
    "moving" = "life_change", 
    "small_business" = "life_change", 
    "wedding" = "life_change",
    "house" = "home_related", 
    "home_improvement" = "home_related")

ggplot(lendingclub, aes(x = purpose_recode)) + 
    geom_bar() + coord_flip()

# ANOVA!!

# Remember that for an ANOVA test, the null hypothesis will be that all of the mean funded amounts are equal across
#the levels of purpose_recode. The alternative hypothesis is that at least one level of purpose_recode has a different mean.
# We will not be sure which, however, without some post hoc analysis, so it will be helpful to know how ANOVA results get stored as an object in R.

lendingclub_lm <- lm(funded_amnt ~ purpose_recode , data = lendingclub)

summary(lendingclub_lm)

anova(lendingclub_lm)

# Based one the very low p-value, purpose recode anova results indicate that there is evidence
# to support the hypothesis that the mean load amounts are different for at least one combination
# of purpose_recode's levels.

# Which loan purpose mean is different?

# The result of that ANOVA test was statistically significant with a very low p-value.
# This means we can reject the null hypothesis and accept the alternative hypothesis that at least one mean was different. But which one?
# We should use Tukey's HSD test, which stands for Honest Significant Difference.

purpose_anova <- aov(funded_amnt ~ purpose_recode , data = lendingclub) #another way to do ANOVA

summary(purpose_anova)

hsd_purpose<- TukeyHSD(purpose_anova, "purpose_recode", conf.level = 0.95)

broom::tidy(hsd_purpose) %>% arrange(adj.p.value)

# Looking at the p-values for each comparison of the levels of purpose_recode, we can see that only
# a few of the mean differences are statistically significant, for example the differences in the means
# for the debt_related and big_purchase loan amounts. In this case, these tiny p-values are most likely
# to be due to large sample size, and further tests would be required to determine what's actually significant
# in the case of loans (known as the practical significance.)


# We can examine more than one explanatory factor in a multiple factor experiment. Let's see how p-values changes after control emp_length.

purpose_emp_anova<- aov(funded_amnt ~ purpose_recode + emp_length, data = lendingclub)

summary(purpose_emp_anova)

hsd_purpose_emp<- TukeyHSD(purpose_emp_anova, "purpose_recode", conf.level = 0.95)

broom::tidy(hsd_purpose_emp) %>% arrange(adj.p.value)

#------------------------------------

# Pre-modeling EDA

# Let's do some EDA with our experiment in mind. Lending Club has now asked you, their data scientist,
# to examine what effect their Lending Club-assigned loan grade variable has on the interest rate, int_rate.
# They're interested to see if the grade they assign the applicant during the process of applying for the loan affects
# the interest rate ultimately assigned to the applicant during the repayment process.


summary(lendingclub$int_rate)

lendingclub %>% 
    group_by(grade) %>% 
        summarize(median_int_rate = median(int_rate),
                  mean_int_rate = mean(int_rate),
                  var_int_rate = var(int_rate))

ggplot(lendingclub, aes(x = grade, y = int_rate)) + 
    geom_boxplot() + 
        labs(title = "Int Rate by Grade")

grade_anova <- aov(int_rate ~ grade, data = lendingclub)

summary(grade_anova)

# The numeric summary and the boxplot that grade seems to heavily influence interest rate. Therefore, the linear model results
# indicating that int_rate is significantly different by grade are unsurprising.

#--------------Post-modeling validation plots + variance

# Another assumption of ANOVA and linear modeling is homogeneity of variance. Homogeneity means "same", and here that would mean 
# that the variance of int_rate is the same for each level of grade. We can test for homogeneity of variances using bartlett.test(),
# which takes a formula and a dataset as inputs.

par(mfrow = c(2,2))
plot(grade_anova)

bartlett.test(int_rate ~ grade, data = lendingclub)


# The residuals on this model are okay, though the residuals on G have a much smaller range than any other level of grade (the dots are 
# far less spread out.) The Q-Q plot, however, shows that the residuals are fairly normal. However, given the highly significant p-value 
# from Bartlett's test, the assumption of homogeneity of variances is violated, which is one of the assumptions of an ANOVA model. Therefore, 
# ANOVA might not be the best choice for this experiment. It happens!

# Alternative

#  One non-parametric alternative to ANOVA is the Kruskal-Wallis rank sum test. For those with some statistics knowledge, it is an extension 
# of the Mann-Whitney U test for when there are more than two groups, like with our grade variable. For us, the null hypothesis for this test
# would be that all of the int_rates have the same ranking by grade.

kruskal.test(int_rate ~ grade, data = lendingclub)

# The low p-value indicates that based on this test, we can be confident in our result, which we found across this experiment, that int_rate varies by grade.
#-----------------------------------------------------------
# A/B TEST!!!

# Sample size for A/B test
# We know now that we need to analyze our A/B test results with a t-test after we've collected data. We have two pretty important questions we need to answer
# before we do that: what's the effect size and what's the sample size required for this test?
# In this case, effect size was given to us. Lending Club is looking to detect the relatively small effect size of 0.2.

pwr.t.test(n = NULL, d = 0.2, sig.level = 0.05, power = 0.8,
           type = "two.sample", alternative = "two.sided")

# Design of A/B Test

# When applicants were using the Lending Club website, they were randomly assigned to two groups, A or B, where A was shown a mint green website header and B 
# was shown a light blue website header. Lending Club was interested to see if website header color choice influenced loan_amnt, the amount an applicant asked to borrow.


# We know the sample size required, and we allowed the experiment to run long enough to get at least 400 people in each group, we can analyze our A/B test.

group <- c( rep("A",500), rep("B", 500))

lendingclub_ab <- cbind(lendingclub[1:1000,], group) # New dataset with group column.

# Plop A/B results

ggplot(lendingclub_ab, aes(x = group, y = loan_amnt)) + 
    geom_boxplot()

t.test(loan_amnt ~ group, data = lendingclub_ab, alternative = "two.sided")


# By looking at both the boxplot and the results of the t-test, it seems that there is no compelling evidence to support the hypothesis that there is a difference
# the two A/B test groups' mean loan_amnt, a result which you would use to help make data-driven decisions at Lending Club.
# The point of an A/B test is that only one thing is changed and the effect of that change is measured.

# A Lending Club multivariate test can combine all of the explanatory variables

lendingclub_multi <- lm(loan_amnt ~ group + grade + verification_status, data = lendingclub_ab)

broom::tidy(lendingclub_multi)

# From the results, verification status and having an F grade are the factors in this model that have a significant effect on loan amount.
#---------------------------------------------

# NHANES Data description: 

# NHANES is a nationally-representative study of adults and children in the United States. Data collection includes in-depth, in-person surveys,
#physical and physiological examinations, and laboratory tests.

# Website : https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2015

nhanes_bodymeasures <- read.csv("https://assets.datacamp.com/production/repositories/1793/datasets/ee832ef6c2fa7036704c53e90dc1e710a3b50dbc/nhanes_bodymeasures.csv")
nhanes_demo <- read.csv("https://assets.datacamp.com/production/repositories/1793/datasets/2be5ca94453a63e825bc30ccefd1429b7683c19c/nhanes_demo.csv") 
nhanes_medical <- read.csv("https://assets.datacamp.com/production/repositories/1793/datasets/d34921a9255422617cdc42f6a3fbcd189f51c19d/nhanes_medicalconditions.csv")

nhanes_combined <- list(nhanes_demo, nhanes_medical, nhanes_bodymeasures) %>%
    Reduce(function(df1, df2) inner_join(df1, df2, by = "seqn"), .)

glimpse(nhanes_combined)

# nhanes EDA

nhanes_combined %>% 
    group_by(mcq365d) %>% 
        summarize(mean_bmxwt = mean(bmxwt, na.rm = T))

nhanes_combined %>% 
    ggplot(aes(as.factor(mcq365d), bmxwt)) +
    geom_boxplot() +
    labs(x = "Treatment",
         y = "Weight")

# Namely that children weren't given the treatment - that's why we see an NA age category.
# We also have some patients have weights missing, thus the warning that the boxplot throws.

nhanes_filter <- nhanes_combined %>% filter(ridageyr > 16)

library(simputation)

nhanes_final <- impute_median(dat = nhanes_filter, bmxwt ~ riagendr) %>% select(bmxwt,mcq365d,riagendr)

nhanes_final$mcq365d <- recode(nhanes_final$mcq365d, 
                               `1` = 1,
                               `2` = 2,
                               `9` = 2)


naniar::miss_var_summary(nhanes_final)

# Randomized Complete Block Designs (RCBD)
 # Description

#A Randomized Complete Block Design (RCBD) is defined by an experiment whose treatment combinations
# are assigned randomly to the experimental units within a block. Generally, blocks cannot be randomized
# as the blocks represent factors with restrictions in randomizations such as location, place, time, gender,
# ethnicity, breeds, etc. It is not simply possible to randomly assign a particular gender to a person. 
# It is not possible to pick a country and call X country. However, the presence of these factors (also 
# known as nuisance factors) will introduce systematic variation in the study.

# Randomized Complete Block Design (RCBD) is arguably the most common design of experiments in many disciplines, 
# including agriculture, engineering, medical, etc. In addition to the experimental error reducing ability, the 
# design widens the generalization of the study findings. For example, if the study contains the place as a blocking 
# factor, the results could be generalized for the places. A fertilizer producer can only claim that it is effective 
# regardless of the climate conditions when it is tested in various climate conditions. 


# The agricolae package is very helpful when you want to "draw" out the design of an experiment for yourself using R. 
# It can draw many different kinds of experiments, including a randomized complete block design

library(agricolae)

designs <- ls("package:agricolae", pattern = "design")
designs

# Build treats and rep
treats <- LETTERS[1:5]
blocks <- 4

# Build my_design_rcbd and view the sketch
my_design_rcbd <- design.rcbd(treats, r = blocks, seed = 33)
my_design_rcbd$sketch

# In this RCBD, we have 4 blocks (each row of the output). Inside of each block, each treatment "A", "B", "C", and "D" is
# used, because this is a complete design. So if these 4 blocks/rows of the output were four fields of a farmer's,
# they should give the first field the "D" treatment in the first season, then "C", then "A", then "B".

#------------------------------------------------------------------------

# NHANES RCBD

# Recall that our blocked experiment involved a treatment where in the doctor asks the patient to reduce their fat or calories
# in their diet, and we're testing the effect this has on weight (bmxwt). We plan to block by gender, which in the NHANES
# dataset is stored as riagendr. Recall that blocking is done to create experimental groups that are as similar as possible.
# Blocking this experiment by gender means that if we observe an effect of the treatment on bmxwt, it's more likely that the 
# effect was actually due to the treatment versus the individual's gender.

nhanes_rcbd <- aov(bmxwt ~ mcq365d + riagendr, data = nhanes_final)

summary(nhanes_rcbd)

nhanes_final %>% 
    group_by(mcq365d, riagendr) %>% 
    summarize(mean_wt = mean(bmxwt))

# It's pretty clear that there truly is a mean difference in weight by gender, so blocking was a good call for this experiment.
# We also observed a statistically significant effect of the treatment on bmxwt, which we hope is actually a result of the 
# treatment. Now that we have the RCBD down, let's tackle Balanced Incomplete Block Designs (BIBD)

# RCBD Model Validation

# It's a good idea to validate the results. We'll examine the Residuals vs. Fitted and Normal Q-Q plots, though now we'll 
# only see a Constant Leverage plot in place of the other two. A good model has a Q-Q plot showing an approximately normal 
# distribution and no clear patterns across blocks or treatments in the others

# We can also look at Interaction plots. We hope to see parallel lines, no matter which of the block or the treatment is 
# on the x-axis. If they are, they satisfy a key assumption of the RCBD model called Additivity.
 # The additive assumption means the effect of changes in a predictor on a response is independent of the effect(s) of changes in other predictor(s).

# Interaction plots : https://support.minitab.com/en-us/minitab-express/1/help-and-how-to/modeling-statistics/anova/how-to/interaction-plot/interpret-the-results/#:~:text=Use%20an%20interaction%20plot%20to,each%20level%20of%20another%20factor.&text=An%20interaction%20occurs.

par(mfrow = c(2,2))
plot(nhanes_rcbd)

par(mfrow = c(2,1))
with(nhanes_final, interaction.plot(mcq365d, riagendr, bmxwt))
with(nhanes_final, interaction.plot(riagendr, mcq365d, bmxwt))


# The initial diganostic plots show that this model is pretty good but not great - especially at the larger end of the data, the Q-Q plot shows 
# the data might not be normal. The interaction plots show nearly parallel lines, so we can move forward with this model.

#-------------------------------------------------------------

# Balanced Incomplete Block Design (BIBD)BIBD
# Is an incomplete block design where all pairs of treatments occur together within a block an equal number of times.

# It's there a BIBD? 

# let : 
#    * t =  # of treatments
#     * k = # treatments per block
#       * r = # replications (lambda) = r * (k-1) / (t-1)   

# A BIBD is not always possible, for it to be possible lambda must be an integer

lambda <- function(t, k , r){
    return( (r * (k-1)) / (t-1))
}
lambda(t = 2, k = 12, r = 22) # BIBD possible
lambda(t = 3, k = 4, r = 11) # BIBD not possible

# Example 

# Say we want to test the difference between four different wet foods in cats' diets on their kidney function. Cat food, however, 
# is expensive, so we'll only test 3 foods per block to save some money. Our cats will be blocked by color of cat, as we aren't 
# interested in that as part of our experiment. The outcome will be measured blood creatinine level, an indicator of kidney function 
# and dysfunction in cats and humans alike.
 

# Calculate lambda
lambda(t = 4, k = 3,r = 3)

# Sketch
my_design_bibd <- design.bib(LETTERS[1:4], k = 3, seed = 42)
my_design_bibd$sketch

# Data 
creatinine <- c(1.98, 1.97, 2.35, 2.09, 1.87, 1.95, 2.08, 2.01, 1.84, 2.06, 1.97, 2.22)
food <- as.factor(c("A", "C", "D", "A", "B", "C", "B", "C", "D", "A", "B", "D"))
color <- as.factor(rep(c("Black", "White", "Orange", "Spotted"), each = 3))
cat_experiment <- as.data.frame(cbind(creatinine, food, color))

# Create cat_model and examine with summary()
cat_model <- aov(creatinine ~ food + color, data = cat_experiment)
summary(cat_model)

# It seems there are no differences by type of wet food in kidney function.
#--------------------------------
#-----------------------------------------

# The most key aspect of the A/B test is the experimental design. There are many points that I will not explain, such as the relevance of the confusion variables and the effect they can have on the experiment.

#---------------------------------------------------------
# Inference for Categorical Data

# General Social Survey
# The GSS gathers data on contemporary American society in order to monitor and explain trends and constants in attitudes, behaviors, and attributes. ... 
# "The GSS contains a standard core of demographic, behavioral, and attitudinal questions, plus topics of special interest.

gss <- get(load("gss.RData"))
glimpse(gss)
summary(gss)
sapply(gss, n_distinct)
naniar::miss_var_summary(gss2016) %>% head(20) 
gss2016 <- gss %>% filter(year == 2016) %>% drop_na(postlife, cappun, natspac, natarms,happy)
#----------------------------------------------------------
# One of the questions that was asked of respondents was: "Do you believe there is a life after death?"

# Let's see how your sample of Americans responded to this question in 2016.

ggplot(gss2016, aes(x = postlife)) +
    geom_bar()

p_hat <- gss2016 %>%  
            summarize(prop_yes = mean(postlife == "YES")) %>% pull()
p_hat
# Generating from H0

# Imagine that when reading the newspaper, you come across someone who makes the following claim: "3/4 of all Americans believe in life after death".
# This can be interpreted as a point null hypothesis that the population proportion has a value of 0.75.

null <- gss2016 %>% 
            specify(response = postlife, success = "YES") %>% 
            hypothesize(null = "point", p = 0.75) %>% 
            generate(reps = 1000, type = "simulate") %>% 
            calculate(stat = "prop")

ggplot(null, aes(stat)) +
    geom_density() +
    geom_vline(xintercept = p_hat, color = "red")

null %>%
    summarize(two_tailed_pval = mean(stat >= p_hat)*2) %>%
        pull(two_tailed_pval)

# The p-value < alpha, thus the data is inconsistent with the null hypothesis so I reject it as a reasonable explanation.

# Death penalty and sex
# While you're on the topic of death and the afterlife, take a look at another question from the GSS:

#Do you favor or oppose the death penalty for people convicted of murder?

# The objective here is to explore if opinions on capital punishment (cappun) diverged between men and women in the gss2016 data.

ggplot(gss2016, aes(x = cappun, fill = sex)) +
    geom_bar(position = "fill")

gss2016 %>% group_by(sex) %>% 
    summarize(prop_FAVOR = mean(cappun == "FAVOR"))

p_hats <- gss2016 %>% group_by(sex) %>% 
            summarize(prop_FAVOR = mean(cappun == "FAVOR")) %>% pull()

d_hat <- diff(p_hats)

# R will do operations like this alphabetically, so -0.09964612 is the proportion of (f)emales that favor minus the proportion of (m)ales.

# We learned that about 56% of women favor the death penalty while about 66% of men do, a difference of about 10 percentage points. That seems like a large difference, but what if 
# it's just due to chance and in fact there is no relationship between sex and support for the death penalty?


null<- gss2016 %>% 
            specify(cappun ~ sex, success = "FAVOR") %>% 
            hypothesize(null = "independence") %>% 
            generate(reps = 1000, type = "permute") %>%
            calculate(stat = "diff in props", order = c("FEMALE", "MALE"))

ggplot(null, aes(stat)) +
    geom_density() +
    geom_vline(xintercept = d_hat, color = "red")

null %>%
    summarize(two_tailed_pval = mean(stat >= p_hat)*2) %>%
    pull(two_tailed_pval)

# The data are strong evidence that there is an association. Means that there is a difference between men and women in the opinion about the capinal punishment

# Confidence Interval
boot <- gss2016 %>%
    specify(cappun ~ sex, success = "FAVOR") %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "diff in props", order = c("FEMALE", "MALE"))

SE <- boot %>%
    summarize(se = sd(stat)) %>%
    pull()

ci <- c(d_hat - (2*SE), d_hat + (2*SE))
ci
boot %>% visualise() + 
    shade_ci(ci)

# CI not included the value of zero, indicating that it is a not plausible value. Leading the same conclusion as the hypothesis test..
#-----------------------------------------------

# Contingency Tables and Chip-square.

# The Chi-Square test of independence is used to determine if there is a significant relationship between two nominal (categorical) variables.  The frequency of each category for one nominal variable is compared across the categories of the second nominal variable.  The data can be displayed in a contingency table where each row represents a category for one variable and each column represents a category for the other variable.  For example, say a researcher wants to examine the relationship between gender (male vs. female) and empathy (high vs. low).  The chi-square test of independence can be used to examine this relationship.  The null hypothesis for this test is that there is no relationship between gender and empathy.  The alternative hypothesis is that there is a relationship between gender and empathy (e.g. there are more high-empathy females than high-empathy males).
# More info: https://www.statisticssolutions.com/non-parametric-analysis-chi-square/#:~:text=The%20Chi%2DSquare%20test%20of,two%20nominal%20(categorical)%20variables.&text=The%20chi%2Dsquare%20test%20of%20independence%20can%20be%20used%20to,relationship%20between%20gender%20and%20empathy.


# Politics and Space.
# What about the relationship between political party and another spending priority: space exploration?


gss2016$party <- recode(gss2016$partyid,
       "STRONG DEMOCRAT" = "Dem",
       "NOT STR DEMOCRAT" = "Dem",
       "IND,NEAR DEM" = "Ind",
       "INDEPENDENT" = "Ind",
       "IND,NEAR REP" = "Ind",
       "NOT STR REPUBLICAN" = "Rep",
       "STRONG REPUBLICAN" = "Rep",
       "OTHER PARTY" = "Oth")

gss_party <- gss2016 %>% filter(party != "Oth")
gss_party$party <- droplevels(gss_party$party, "Oth")

ggplot(gss_party, aes(party, fill = natspac)) + 
    geom_bar() + 
        labs(
            title = "Visual representation of a contingency table")

# natspac ~ party
chi_obs_spac <- gss_party %>% chisq_stat(natspac ~ party)

null_spac <- gss_party %>%
    specify(natspac ~ party) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 1000, type = "permute") %>%
    calculate(stat = "Chisq")

ggplot(null_spac, aes(x = stat)) +
    geom_density() +
    geom_vline(xintercept = chi_obs_spac, color = "red")

null_spac %>% 
    summarize(p_val = mean(stat >= chi_obs_spac))

# natarms ~ party

gss_party %>% select(natarms, party) %>% table()

chi_obs_arms <- gss_party %>% chisq_stat(natarms ~ party)

null_arms <- gss_party %>%
    specify(natarms ~ party) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 1000, type = "permute") %>%
    calculate(stat = "Chisq")

ggplot(null_arms, aes(x = stat)) +
    geom_density() +
    geom_vline(xintercept = chi_obs_arms, color = "red")

null_arms %>% 
    summarize(p_val = mean(stat >= chi_obs_arms))

# The data set is consistent with the hypothesis that there is no relationship between political party and space exploration spending, but does suggestion a relationship between party and spending on the military.

#-----------------------------------------------------------
# Alternate method: the chi-squared distribution

# Approximation distributions: chi-squared

    # Shape is determiden by degrees of freedom 
    # df = (nrows- 1) x (ncols - 1)
# Becomes a good approximation when: 
    # Expected_count >= 5 (in each cell)
    # df >= 2
    # large sample size 
#----------------------------------
# Region ~ Happy

gss2016_RH <- gss2016 %>% select(region, happy)
gss2016_RH$happy <- recode(gss2016_RH$happy,
                           "VERY HAPPY" = "HAPPY",
                           "PRETTY HAPPY" = "HAPPY",
                           "NOT TOO HAPPY" = "UNHAPPY") 

table(gss2016_RH)

degrees_of_freedom <- (nrow(table(gss2016_RH)) - 1) * (ncol(table(gss2016_RH)) - 1) 

#----------------------------

ggplot(gss2016_RH, aes(y = region, fill = happy)) + 
    geom_bar(position = "fill") 
chi_obs <- gss2016_RH %>% chisq_stat(region ~ happy)

null <- gss2016_RH %>%
    specify(happy ~ region, success = "HAPPY") %>%
    hypothesize(null = "independence") %>%
    generate(reps = 500, type = "permute") %>%
    calculate(stat = "Chisq")

ggplot(null, aes(stat)) + 
    geom_density(lwd = 2) +
    geom_vline(xintercept = chi_obs, color = "red") + 
    stat_function(fun = dchisq, args = list(df = degrees_of_freedom), color = "blue", lwd = 2)

# Calculate computational pval
null %>%
    summarize(pval = mean(stat >= chi_obs))

# Calculate approximate pval
pchisq(chi_obs, df = degrees_of_freedom, lower.tail = FALSE)


