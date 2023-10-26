# Script to replicate analyses in paper by Goldberg et al. 2023
# doi of paper: 10.1257/app.20200721

# Data were downloaded from replication package available at https://www.aeaweb.org/articles?id=10.1257/app.20200721


### PREPARATORY STEPS

# 1. Require packages
require(haven) # To read dta files
require(nlme) # To do mixed-effects regression
require(performance) # To check model assumptions


# 2. Read data
TB_data <- read_dta("C:/Users/gusta/Box Sync/Gustavs_arbete/Arkiverat/Replication games 2023/150781-V1/TB_data.dta")


# 3. Clean data 
TB_data <- subset(TB_data, subset = TB_data$bl_survey_status_cleaned == 1) 
# This retains 3402 out of 4203 observations
# According to original code comment, this is to "keep only CPs who were surveyed"

TB_data <- subset(TB_data, subset = TB_data$id_city != 10) 
# This retains 3176 out of 3402 observations
# According to original code comment, this is to "drop Bhubaneshwar centers" for reasons of an agreement


# 4. Check for verification: replicate numbers in table 1
table(TB_data$treatment_group)
# Numbers of patients in each group matches table 1
# Numbers of centers not checked


# 5. Define variables for analysis
TB_data$t_incentive_encouragement <- as.factor(TB_data$t_incentive_encouragement)
TB_data$t_incentive_unconditional <- as.factor(TB_data$t_incentive_unconditional)
TB_data$t_incentive_conditional <- as.factor(TB_data$t_incentive_conditional)
TB_data$id_city <- as.factor(TB_data$id_city)



### Analyses

# 6. Regressions for table 2 column 1
mod1 <- lme(a_num_ref_visit ~ t_incentive_encouragement + t_incentive_unconditional + t_incentive_conditional + id_city, random = ~ 1 | id_centre, data = TB_data)
summary(mod1)
check_model(mod1)

# It turns out that model fit is pretty poor
# The reason appears to be that data are massively zero-inflated
# We can try modelling it in a way that takes this into account
# Another extension would be to model nested random effects for centers in cities instead of using city as dummy variable
hist(TB_data$a_num_ref_visit)
table(TB_data$a_num_ref_visit)

# Regressions for table 2 column 2
mod2 <- lme(a_num_rec_test ~ t_incentive_encouragement + t_incentive_unconditional + t_incentive_conditional + id_city, random = ~ 1 | id_centre, data = TB_data)
summary(mod2)
check_model(mod2)

# Regressions for table 2 column 3
mod3 <- lme(admin_num_tested ~ t_incentive_encouragement + t_incentive_unconditional + t_incentive_conditional + id_city, random = ~ 1 | id_centre, data = TB_data)
summary(mod3)
check_model(mod3)

# Regressions for table 2 column 4
mod4 <- lme(a_num_pos ~ t_incentive_encouragement + t_incentive_unconditional + t_incentive_conditional + id_city, random = ~ 1 | id_centre, data = TB_data)
summary(mod4)
check_model(mod4)

# Regressions for table 3 column 1
mod5 <- lme(a_num_ref_visit ~ t_outreach_peer + t_outreach_identified + t_outreach_anonymous + id_city, random = ~ 1 | id_centre, data = TB_data)
summary(mod5)
check_model(mod5)

# Regressions for table 3 column 2
mod6 <- lme(a_num_rec_test ~ t_outreach_peer + t_outreach_identified + t_outreach_anonymous + id_city, random = ~ 1 | id_centre, data = TB_data)
summary(mod6)
check_model(mod6)

# Regressions for table 3 column 3
mod7 <- lme(admin_num_tested ~ t_outreach_peer + t_outreach_identified + t_outreach_anonymous + id_city, random = ~ 1 | id_centre, data = TB_data)
summary(mod7)
check_model(mod7)

# Regressions for table 3 column 4
mod8 <- lme(a_num_pos ~ t_outreach_peer + t_outreach_identified + t_outreach_anonymous + id_city, random = ~ 1 | id_centre, data = TB_data)
summary(mod8)
check_model(mod8)


