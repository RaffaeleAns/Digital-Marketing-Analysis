#### LIBRARIES ####
library(dplyr)
library(magrittr)
library(ggplot2)

#---------------------------#
#### IMPORTING DATA SETS ####
#---------------------------#

### set working directory ###

### clients fidelity subscriptions ###
df_1_cli_fid <- read.csv2("raw_1_cli_fid.csv", na.strings = c("NA", ""))

### clients accounts details ###
df_2_cli_account <- read.csv2("raw_2_cli_account.csv", na.strings = c("NA", ""))

### clients addresses ###
df_3_cli_address <- read.csv2("raw_3_cli_address.csv", na.strings = c(""), stringsAsFactors = F)

### clients privacy ###
df_4_cli_privacy <- read.csv2("raw_4_cli_privacy.csv" , na.strings = c("NA", ""))

### email campaign characterization ###
df_5_camp_cat <- read.csv2("raw_5_camp_cat.csv" , na.strings = c("NA", ""))

### email event ###
df_6_camp_event <- read.csv2("raw_6_camp_event.csv" , na.strings = c("NA", ""))

### scontrini ###
df_7_tic <- read.csv2("raw_7_tic.csv", na.strings = c("NA", ""))


#---------------------#
#### DATA CLEANING ####
#---------------------#

### df_1_cli_fid ###

## first look ##
str(df_1_cli_fid)
summary(df_1_cli_fid)

## cleaning ##
df_1_cli_fid_clean <- df_1_cli_fid

## formatting dates ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(DT_ACTIVE = as.Date(DT_ACTIVE))

## formatting numerical categories as factor ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(ID_NEG = as.factor(ID_NEG)) %>%
  mutate(TYP_CLI_FID = as.factor(TYP_CLI_FID)) %>%
  mutate(STATUS_FID = as.factor(STATUS_FID))

## (consistency control) number of fid per client ##
num_fid_x_cli <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  summarize(NUM_FIDs =  n_distinct(ID_FID), NUM_DATEs = n_distinct(DT_ACTIVE))

dist_num_fid_x_cli <- num_fid_x_cli %>%
  group_by(NUM_FIDs, NUM_DATEs) %>%
  summarize(TOT_CLIs = n_distinct(df_1_cli_fid_clean$ID_CLI))

dist_num_fid_x_cli

# there are clients with multiple fid
# lets have a closer look
num_fid_x_cli %>% filter(NUM_DATEs == 3)

df_1_cli_fid %>% filter(ID_CLI == 621814)

## keep both first fid and last fid ##
# first --> registration date
# last --> features
df_1_cli_fid_first <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == min(DT_ACTIVE)) %>%
  arrange(ID_FID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_last <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == max(DT_ACTIVE)) %>%
  arrange(desc(ID_FID)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_clean <- df_1_cli_fid_last %>%
  left_join(df_1_cli_fid_first %>%
              select(ID_CLI, FIRST_ID_NEG = ID_NEG, FIRST_DT_ACTIVE = DT_ACTIVE)
            , by = 'ID_CLI') %>%
  left_join(num_fid_x_cli %>%
              select(ID_CLI, NUM_FIDs) %>%
              mutate(NUM_FIDs = as.factor(NUM_FIDs))
            , by = 'ID_CLI')

## explore distributions ##
# COD_FID
df_1_cli_fid_clean %>%
  group_by(COD_FID) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_1_cli_fid_clean, aes(x=COD_FID)) + geom_bar()

# TYP_CLI_FID
df_1_cli_fid_clean %>%
  group_by(TYP_CLI_FID) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_1_cli_fid_clean, aes(x=TYP_CLI_FID)) + geom_bar()

# STATUS_FID
df_1_cli_fid_clean %>%
  group_by(STATUS_FID) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_1_cli_fid_clean, aes(x=STATUS_FID)) + geom_bar()

# ID_NEG
df_1_cli_fid_clean %>%
  group_by(ID_NEG) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs))  %>%
  arrange(desc(PERCENT))

ggplot(df_1_cli_fid_clean, aes(x=ID_NEG)) + geom_bar()



### df_2_cli_account ###

## first look ##
str(df_2_cli_account)
summary(df_2_cli_account)

## cleaning ##
df_2_cli_account_clean <- df_2_cli_account

## formatting boolean as factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = as.factor(W_PHONE))

## formatting numerical categories as factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(TYP_CLI_ACCOUNT = as.factor(TYP_CLI_ACCOUNT))

## correct NA in categories ##
# we make use of the package forcats
library(forcats)

df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = fct_explicit_na(W_PHONE, "0")) %>%
  mutate(EMAIL_PROVIDER = fct_explicit_na(EMAIL_PROVIDER, "(missing)")) %>%
  mutate(TYP_JOB = fct_explicit_na(TYP_JOB, "(missing)"))

## explore distributions ##
# COD_FID
df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df_2_cli_account_clean %>%
  summarize(TOT_EMAIL_PROVIDER = n_distinct(EMAIL_PROVIDER))

# too many different values for EMAIL_PROVIDER to be an useful category

# W_PHONE
df_2_cli_account_clean %>%
  group_by(W_PHONE) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_2_cli_account_clean, aes(x=W_PHONE)) + geom_bar()

# TYP_JOB
df_2_cli_account_clean %>%
  group_by(TYP_JOB) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df_2_cli_account_clean %>%
  summarize(TOT_TYP_JOB = n_distinct(TYP_JOB))

ggplot(df_2_cli_account_clean, aes(x=TYP_JOB)) + geom_bar()

## lets review ##
str(df_2_cli_account_clean)
summary(df_2_cli_account_clean)

# too many missing values for EMAIL_PROVIDER to be an useful category
# keep the most frequent values and (missing) while changing the remaing into "OTHER"
freq_email_providers <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs))

head(freq_email_providers, 20)

clean_email_providers <- freq_email_providers %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  mutate(AUX = if_else(PERCENT_COVERED < 0.85 | (PERCENT_COVERED > 0.85 & lag(PERCENT_COVERED) < 0.85), 1,0)) %>%
  mutate(EMAIL_PROVIDER_CLEAN = if_else(AUX | EMAIL_PROVIDER == "(missing)", EMAIL_PROVIDER, "others"))

head(clean_email_providers, 20)

df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  left_join(clean_email_providers %>%
              select(EMAIL_PROVIDER, EMAIL_PROVIDER_CLEAN)
            , by = "EMAIL_PROVIDER") %>%
  select(-EMAIL_PROVIDER) %>%
  mutate(EMAIL_PROVIDER_CLEAN = as.factor(EMAIL_PROVIDER_CLEAN))

## explore distributions ##
# EMAIL_PROVIDER_CLEAN
df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER_CLEAN) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_2_cli_account_clean, aes(x=EMAIL_PROVIDER_CLEAN)) + geom_bar()

## lets review ##
str(df_2_cli_account_clean)
summary(df_2_cli_account_clean)



### df_3_cli_address ###

## first look ##
str(df_3_cli_address)
summary(df_3_cli_address)

## cleaning ##
df_3_cli_address_clean <- df_3_cli_address

## convert PRV e REGION into factors ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  mutate(PRV = as.factor(PRV)) %>%
  mutate(REGION = as.factor(REGION)) %>%
  distinct()

# closer look on df_3_cli_address
df_3_cli_address_clean %>%
  group_by(w_CAP = !is.na(CAP), w_PRV = !is.na(PRV), w_REGION = !is.na(REGION)) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS))

# drop the record without CAP - PRV - REGION
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  filter(!is.na(CAP) & !is.na(PRV) & !is.na(REGION))

## explore distributions ##
# PRV
df_3_cli_address_clean %>%
  group_by(PRV) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_ADDs/sum(TOT_ADDs)) %>%
  arrange(desc(PERCENT))

ggplot(df_3_cli_address_clean, aes(x=PRV)) + geom_bar()

# REGION
df_3_cli_address_clean %>%
  group_by(REGION) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_ADDs/sum(TOT_ADDs)) %>%
  arrange(desc(PERCENT))

ggplot(df_3_cli_address_clean, aes(x=REGION)) + geom_bar()

## lets review ##
str(df_3_cli_address_clean)
summary(df_3_cli_address_clean)



### df_4_cli_privacy ###

## first look ##
str(df_4_cli_privacy)
summary(df_4_cli_privacy)

## cleaning ##
df_4_cli_privacy_clean <- df_4_cli_privacy

# formatting boolean into facotr
df_4_cli_privacy_clean <- df_4_cli_privacy_clean %>%
  mutate(FLAG_PRIVACY_1 = as.factor(FLAG_PRIVACY_1)) %>%
  mutate(FLAG_PRIVACY_2 = as.factor(FLAG_PRIVACY_2)) %>%
  mutate(FLAG_DIRECT_MKT = as.factor(FLAG_DIRECT_MKT))

## explore distributions ##
# FLAG_PRIVACY_1
df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_1) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_4_cli_privacy_clean, aes(x=FLAG_PRIVACY_1)) + geom_bar()

# FLAG_PRIVACY_2
df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_2) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_4_cli_privacy_clean, aes(x=FLAG_PRIVACY_2)) + geom_bar()

# FLAG_DIRECT_MKT
df_4_cli_privacy_clean %>%
  group_by(FLAG_DIRECT_MKT) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_4_cli_privacy_clean, aes(x=FLAG_DIRECT_MKT)) + geom_bar()

df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_1, FLAG_PRIVACY_2, FLAG_DIRECT_MKT) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

## lets review ##
str(df_4_cli_privacy_clean)
summary(df_4_cli_privacy_clean)




### df_5_camp_cat ###

## first look ##
str(df_5_camp_cat)
summary(df_5_camp_cat)

## cleaning ##
df_5_camp_cat_clean <- df_5_camp_cat

# the field CHANNEL_CAMP has one value <-- is not relevant
df_5_camp_cat_clean <- df_5_camp_cat_clean %>%
  select(-CHANNEL_CAMP)

## explore distributions ##
# FLAG_DIRECT_MKT
df_5_camp_cat_clean %>%
  group_by(TYP_CAMP) %>%
  summarize(TOT_CAMPs = n_distinct(ID_CAMP)) %>%
  mutate(PERCENT = TOT_CAMPs/sum(TOT_CAMPs)) %>%
  arrange(desc(PERCENT))

ggplot(df_5_camp_cat_clean, aes(x=TYP_CAMP)) + geom_bar()

summary(df_5_camp_cat_clean)




### df_6_camp_event ###

## first look ##
str(df_6_camp_event)
summary(df_6_camp_event)

## cleaning ##
df_6_camp_event_clean <- df_6_camp_event

# despite the field EVENT_TIME is datetime, we just need the corresponding dates
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATE, format="%Y-%m-%dT%H:%M:%S"))

# for the purpose of the analysis we are delivering here it would make no difference distinguish "ERRORS" and "BOUNCE"
# lets combine them into a common category "FAILURE" with "F" as EVENT_CODE before changing the field to factor
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(TYP_EVENT = as.factor(if_else(TYP_EVENT == "E" | TYP_EVENT == "B", "F", as.character(TYP_EVENT))))

## explore distributions ##
# type event
df_6_camp_event_clean %>%
  group_by(TYP_EVENT) %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT), TOT_CLIs = n_distinct(ID_CLI), TOT_CAMPs = n_distinct(ID_CAMP)) %>%
  mutate(PERCENT_EVENT = TOT_EVENTs/sum(TOT_EVENTs), PERCENT_CLI = TOT_CLIs/sum(TOT_CLIs), PERCENT_CAMP = TOT_CAMPs/sum(TOT_CAMPs)) %>%
  arrange(desc(PERCENT_EVENT), desc(PERCENT_EVENT), desc(PERCENT_CAMP))

ggplot(df_6_camp_event_clean %>% select(TYP_EVENT, ID_EVENT) %>% distinct(), aes(x=TYP_EVENT)) + geom_bar()
ggplot(df_6_camp_event_clean %>% select(TYP_EVENT, ID_CLI) %>% distinct(), aes(x=TYP_EVENT)) + geom_bar()
ggplot(df_6_camp_event_clean %>% select(TYP_EVENT, ID_CAMP) %>% distinct(), aes(x=TYP_EVENT)) + geom_bar()

# min - max dates
df_6_camp_event_clean %>% summarize(MIN_DATE = min(EVENT_DATE), MAX_DATE = max(EVENT_DATE))





### df_7_tic ###
# despite the field EVENT_TIME is datetime, we just need the corresponding dates
df_7_tic_clean <- df_7_tic %>%
  mutate(DATETIME = as.Date(DATETIME, format="%Y-%m-%d"))

## formatting boolean as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(DIREZIONE = as.factor(DIREZIONE))

## formatting numerical categories as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(COD_REPARTO = as.factor(COD_REPARTO))


#------------------------#
#### DATA PREPARATION ####
#------------------------#

# the aim is to create what we need for our model.

# first we explore the distribution
df_6_camp_event_clean_w_type <- df_6_camp_event_clean %>%
  left_join(df_5_camp_cat_clean
            , by = "ID_CAMP")

# send
df_sents <- df_6_camp_event_clean_w_type %>%
  filter(TYP_EVENT == "S") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_S = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, SEND_DATE = EVENT_DATE)

# open
df_opens <- df_6_camp_event_clean_w_type %>%
  filter(TYP_EVENT == "V") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_O = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, OPEN_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(OPEN_DATE == min(OPEN_DATE)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# click
df_clicks <- df_6_camp_event_clean_w_type %>%
  filter(TYP_EVENT == "C") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_C = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, CLICK_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(CLICK_DATE == min(CLICK_DATE)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# failure
df_fails <- df_6_camp_event_clean_w_type %>%
  filter(TYP_EVENT == "F") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_F = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, FAIL_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(FAIL_DATE == min(FAIL_DATE)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# attach send to open
df_sents_w_open <- df_sents %>%
  left_join(df_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(OPEN_DATE) | SEND_DATE <= OPEN_DATE) %>%
  mutate(DIFF = as.integer(OPEN_DATE - SEND_DATE))

# number of sents without opens
df_sents_w_open %>%
  group_by(w_open = !is.na(DIFF)) %>%
  summarize(TOT_SENTs = n_distinct(ID_EVENT_S)) %>%
  mutate(PERCENT = TOT_SENTs/sum(TOT_SENTs)) %>%
  arrange(desc(PERCENT))

ggplot(df_sents_w_open, aes(x=!is.na(DIFF))) + geom_bar()

# distribution days opens
df_sents_w_open %>% filter(!is.na(DIFF)) %>%
  group_by(DIFF) %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)) %>%
  arrange(DIFF) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_EVENTs)/sum(TOT_EVENTs))

ggplot(df_sents_w_open %>% filter(!is.na(DIFF)) %>%
         group_by(DIFF) %>%
         summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)) %>%
         arrange(DIFF) %>%
         mutate(PERCENT_COVERED = cumsum(TOT_EVENTs)/sum(TOT_EVENTs)) %>%
         filter(DIFF <= 14)
       , aes(y=PERCENT_COVERED, x=DIFF)) + geom_line() + geom_point() + scale_x_continuous(breaks=seq(0,14,2), minor_breaks=0:14)

# we can choose as window function 2 day
window_days <- 2

### construction of the datamart ###

# our target variable will be if a send event is open within the timespan of the window days

target_event <- df_sents_w_open %>%
  mutate(TARGET = as.factor(if_else(!is.na(DIFF) & DIFF <= window_days, "1", "0"))) %>%
  select(ID_EVENT_S, ID_CLI, ID_CAMP, ID_DELIVERY, SEND_DATE, TARGET)

# some relavant variable we want to include are:
# - average open rate (within 14 days) of the communications received by the client in the 30 days before the sent
# - average click-through (within 14 days) rate of the communications received by the client in the 30 days before the sent

# in order to have comparable situation we are considering:
# - targeted sent made after the 2019-02-01 and window_days before 2019-04-30
# - targeted sent to clients registered by at least 30 days

rate_window <- 14
prev_window <- 30

dt_start <- as.Date("2019-02-01")
dt_end <- as.Date("2019-04-30") - window_days

relevant_event <- df_sents %>%
  left_join(df_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(OPEN_DATE) | SEND_DATE <= OPEN_DATE) %>%
  left_join(df_clicks
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(CLICK_DATE) | SEND_DATE <= CLICK_DATE) %>%
  left_join(df_fails
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(FAIL_DATE) | SEND_DATE <= FAIL_DATE) %>%
  mutate(DIFF_OPEN = as.integer(OPEN_DATE - SEND_DATE)) %>%
  mutate(DIFF_CLICK = as.integer(CLICK_DATE - SEND_DATE)) %>%
  filter(is.na(DIFF_OPEN) | DIFF_OPEN < rate_window) %>%
  filter(is.na(DIFF_CLICK) | DIFF_CLICK < rate_window)

names(relevant_event) <- sapply(names(relevant_event), paste0, "_PREV")

target_event_w_prev <- target_event %>% filter(SEND_DATE >= dt_start & SEND_DATE <= dt_end) %>%
  left_join(relevant_event
            , by = c("ID_CLI" = "ID_CLI_PREV")
  ) %>%
  filter(is.na(SEND_DATE_PREV) | (SEND_DATE_PREV < SEND_DATE & SEND_DATE <= SEND_DATE_PREV + prev_window)) %>%
  mutate(OPENED = if_else(OPEN_DATE_PREV <= SEND_DATE & SEND_DATE <= OPEN_DATE_PREV + prev_window, 1, 0)) %>%
  mutate(CLICKED = if_else(CLICK_DATE_PREV <= SEND_DATE & SEND_DATE <= CLICK_DATE_PREV + prev_window, 1, 0)) %>%
  mutate(FAILED = if_else(!is.na(ID_EVENT_F_PREV), 1, 0)) %>%
  group_by(ID_EVENT_S, ID_CLI, ID_CAMP, ID_DELIVERY, SEND_DATE,  TARGET) %>%
  summarize(NUM_SEND_PREV = n_distinct(ID_EVENT_S_PREV, na.rm = T)
            , NUM_OPEN_PREV = sum(OPENED, na.rm = T)
            , NUM_CLICK_PREV = sum(CLICKED, na.rm = T)
            , NUM_FAIL_PREV = sum(FAILED, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(OPEN_RATE_PREV = NUM_OPEN_PREV/NUM_SEND_PREV) %>%
  mutate(CLICK_RATE_PREV = NUM_CLICK_PREV/NUM_OPEN_PREV) %>%
  mutate(W_SEND_PREV = as.factor(NUM_SEND_PREV > 0)) %>%
  mutate(W_FAIL_PREV = as.factor(NUM_FAIL_PREV > 0)) %>%
  mutate(SEND_WEEKDAY = as.factor(weekdays(SEND_DATE))) %>%
  mutate(OPEN_RATE_PREV = if_else(is.na(OPEN_RATE_PREV), 0, OPEN_RATE_PREV)) %>%
  mutate(CLICK_RATE_PREV = if_else(is.na(CLICK_RATE_PREV), 0, CLICK_RATE_PREV))


# add client data
df_master <- target_event_w_prev %>%
  left_join(df_1_cli_fid_clean %>%
              select(ID_CLI, ID_NEG, TYP_CLI_FID, COD_FID, STATUS_FID, FIRST_DT_ACTIVE, NUM_FIDs)
            , by = "ID_CLI") %>%
  filter(FIRST_DT_ACTIVE <= SEND_DATE) %>%
  # filter(FIRST_DT_ACTIVE <= SEND_DATE - 30) %>%
  mutate(AGE_FID = as.integer(SEND_DATE - FIRST_DT_ACTIVE)) %>%
  left_join(df_2_cli_account_clean
            , by = "ID_CLI") %>%
  left_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS, PRV, REGION)
            , by = "ID_ADDRESS") %>%
  left_join(df_4_cli_privacy_clean
            , by = "ID_CLI") %>%
  mutate(PRV = fct_explicit_na(PRV)) %>%
  mutate(REGION = fct_explicit_na(REGION)) %>%
  select(-ID_ADDRESS, -ID_CLI, -ID_CAMP, -ID_DELIVERY, -SEND_DATE, -FIRST_DT_ACTIVE)

# check there are not duplicates
df_master %>%
  group_by(ID_EVENT_S) %>% 
  summarize(num = n()) %>% 
  group_by(num) %>%
  count()


#### DATA ESPLORATION ####

# lets see the frequency of the event
df_master %>%
  group_by(TARGET) %>%
  summarize(NUM_EVENTs = n_distinct(ID_EVENT_S))

df_master %>%
  group_by(TARGET,  W_SEND_PREV) %>%
  summarize(NUM_EVENTs = n_distinct(ID_EVENT_S), mean_OR = mean(OPEN_RATE_PREV, na.rm = T))

str(df_master)
summary(df_master)
View(df_master)

### T-TEST ###
# check if a continuous variable has a significative difference
t.test(NUM_SEND_PREV ~ TARGET, data = df_master)
t.test(NUM_OPEN_PREV ~ TARGET, data = df_master)
t.test(NUM_CLICK_PREV ~ TARGET, data = df_master)
t.test(NUM_FAIL_PREV ~ TARGET, data = df_master)
t.test(OPEN_RATE_PREV ~ TARGET, data = df_master)
t.test(CLICK_RATE_PREV ~ TARGET, data = df_master)
t.test(AGE_FID ~ TARGET, data = df_master)


library(tidyr) # needed for the pivot function spread()

prepare_chisq <- function(df, x){
  y <- enquo(x)
  
  test_df <- df %>%
    mutate(KEY = if_else(TARGET == "1", "OK", "KO")) %>%
    select(UQ(y), KEY, ID_EVENT_S) %>%
    group_by(UQ(y), KEY) %>%
    summarize(n = n()) %>%
    spread(KEY, n) %>%
    ungroup() %>%
    as.data.frame()
  
  test_m <- test_df %>%
    select(OK, KO) %>%
    mutate(OK = if_else(is.na(OK), as.integer(0), OK)) %>%
    mutate(KO = if_else(is.na(KO), as.integer(0), KO)) %>%
    as.matrix() 
  row.names(test_m) <- as.character(test_df[,1])
  
  return(test_m)
}

plot_factor <- function(df, x, lab){
  y <- enquo(x)
  
  df_count_tot <- df %>%
    group_by(UQ(y)) %>%
    summarise(n_tot = n_distinct(ID_EVENT_S)) %>%
    ungroup()
  
  df_count <- df %>%
    group_by(UQ(y), TARGET) %>%
    summarise(n = n_distinct(ID_EVENT_S))
  
  df <- df_count %>%
    left_join(df_count_tot, by = lab) %>%
    mutate(frac = round(n / n_tot, 2))
  
  ggplot(data=df, aes(x=UQ(y), y=frac, fill=TARGET)) +
    geom_bar(stat="identity", position=position_dodge()) +
    geom_text(aes(x=UQ(y), y=frac, label = frac),
              position = position_dodge(width = 1),
              vjust = 2, size = 3, color = "white", fontface = "bold")
}

chisq.test(prepare_chisq(df_master, W_SEND_PREV))
plot_factor(df_master, W_SEND_PREV, "W_SEND_PREV")

chisq.test(prepare_chisq(df_master, W_FAIL_PREV))
plot_factor(df_master, W_FAIL_PREV, "W_FAIL_PREV")

chisq.test(prepare_chisq(df_master, SEND_WEEKDAY))
plot_factor(df_master, SEND_WEEKDAY, "SEND_WEEKDAY")

chisq.test(prepare_chisq(df_master, ID_NEG))
plot_factor(df_master, ID_NEG, "ID_NEG")

chisq.test(prepare_chisq(df_master, TYP_CLI_FID))
plot_factor(df_master, TYP_CLI_FID, "TYP_CLI_FID")

chisq.test(prepare_chisq(df_master, COD_FID))
plot_factor(df_master, COD_FID, "COD_FID")

chisq.test(prepare_chisq(df_master, STATUS_FID))
plot_factor(df_master, STATUS_FID, "STATUS_FID")

chisq.test(prepare_chisq(df_master, NUM_FIDs))
plot_factor(df_master, NUM_FIDs, "NUM_FIDs")

chisq.test(prepare_chisq(df_master, W_PHONE))
plot_factor(df_master, W_PHONE, "W_PHONE")

chisq.test(prepare_chisq(df_master, TYP_JOB))
plot_factor(df_master, TYP_JOB, "TYP_JOB")

chisq.test(prepare_chisq(df_master, EMAIL_PROVIDER_CLEAN))
plot_factor(df_master, EMAIL_PROVIDER_CLEAN, "EMAIL_PROVIDER_CLEAN")

chisq.test(prepare_chisq(df_master, PRV))
plot_factor(df_master, PRV, "PRV")

chisq.test(prepare_chisq(df_master, REGION))
plot_factor(df_master, REGION, "REGION")

chisq.test(prepare_chisq(df_master, FLAG_PRIVACY_1))
plot_factor(df_master, FLAG_PRIVACY_1, "FLAG_PRIVACY_1")

chisq.test(prepare_chisq(df_master, FLAG_PRIVACY_2))
plot_factor(df_master, FLAG_PRIVACY_2, "FLAG_PRIVACY_2")

chisq.test(prepare_chisq(df_master, FLAG_DIRECT_MKT))
plot_factor(df_master, FLAG_DIRECT_MKT, "FLAG_DIRECT_MKT")

master_df <- df_master 

#AGGREGAZIONE NA DI EMAIL_PROVIDER IN OTHER
master_df$EMAIL_PROVIDER_CLEAN[is.na(master_df$EMAIL_PROVIDER_CLEAN)] <- "others"


#--------------------------#
#--------------------------#
##### EMAIL ENGAGEMENT #####
#--------------------------#
#--------------------------#



### TRAINING - TEST SET ###
library(caret)
library(InformationValue)
set.seed(12345)
#campionamento stratificato secondo la variabile TARGET (sbilanciata)
training_indices <- master_df$TARGET %>% createDataPartition(p=0.75, list = FALSE)

training_set <- master_df[training_indices, ] 
test_set <- master_df[-training_indices, ]


#MODELLO LOGIT
logit_model<- glm(TARGET ~ NUM_SEND_PREV +  OPEN_RATE_PREV + CLICK_RATE_PREV  + SEND_WEEKDAY + ID_NEG + TYP_CLI_FID + STATUS_FID + NUM_FIDs + AGE_FID + W_PHONE + TYP_CLI_ACCOUNT +  EMAIL_PROVIDER_CLEAN + REGION,
                  family = binomial(link = "logit"),
                  data = training_set)

logit_prob <- plogis(predict(logit_model, test_set))

logit_pred <- ifelse(logit_prob > 0.5, 1, 0)

#ROC Curve
ROC_logit <- plotROC(test_set$TARGET, logit_prob)

#Confusion Matrix and Statistics 
CONFUSION_MATRIX_LOG <- confusionMatrix(test_set$TARGET, logit_pred)
CONFUSION_MATRIX_LOG 


#TREE MODEL

library(tree)
cntrl<- tree.control(nrow(training_set), mincut=30)
tree_model<- tree(TARGET ~ NUM_SEND_PREV +  OPEN_RATE_PREV + CLICK_RATE_PREV  + SEND_WEEKDAY + TYP_CLI_FID + STATUS_FID + NUM_FIDs + AGE_FID + W_PHONE + TYP_CLI_ACCOUNT +  EMAIL_PROVIDER_CLEAN + REGION,
                  data = training_set,
                  control = cntrl)
summary(tree_model)
tree_prob <- plogis(predict(tree_model, test_set))
tree_prob <- tree_prob[ ,2]

optCutOff <- optimalCutoff(test_set$TARGET, tree_prob)
soglia <- as.numeric(optCutOff)

tree_pred <- ifelse(tree_prob > optCutOff, 1, 0)

#ROC Curve
ROC_tree <- plotROC(test_set$TARGET, tree_prob)

#Confusion Matrix and Statistics 
CONFUSION_MATRIX_TREE <- confusionMatrix(test_set$TARGET, tree_pred)
CONFUSION_MATRIX_TREE


#------------------#
#------------------#
#### CLUSTERING ####
#------------------#
#------------------#


### DATA PREPARATION ###

#Numero scontrini per customer
BILLS <- df_7_tic_clean %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarize(TOT_BILLS = n_distinct(ID_SCONTRINO)) 

#gross sales per customer
GROSS <- df_7_tic_clean %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarize(GROSS_SALES = sum(IMPORTO_LORDO)) 

#DISC_TOT = somma totale di sconto
DISCOUNT <- df_7_tic_clean %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  mutate(DISC_TOT = as.numeric(sum(SCONTO))) 

DISCOUNT <- DISCOUNT[ ,c(2,10)]



#FILTRIAMO SOLO GLI SCONTI RITENUTI SIGNIFICATIVI, OVVERO SUPERIORI AI 5 EURO. GLI ALTRI SARANNO CAMBIATI IN 0
DISCOUNT$DISC_TOT[which(DISCOUNT$DISC_TOT < 5)] <- 0

#Articoli venduti per customer
ART_SOLD <- df_7_tic_clean %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarize(ART_SOLD = n_distinct(ID_ARTICOLO)) 

#data primo acquisto per cliente 
TIME_1 <- df_7_tic_clean %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  filter(DATETIME == min(DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

#ultimo acqusto per cliente
TIME_2 <- df_7_tic_clean %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  filter(DATETIME == max(DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()


TIME_1 <- TIME_1[ ,c("ID_CLI","DATETIME")]
TIME_2 <- TIME_2[ ,c("ID_CLI","DATETIME")]

#JOIN PRIMA E ULTIMA DATA DI ACQUISTO
DATASET_1 <- GROSS %>%
  left_join(TIME_1 %>%
              select(ID_CLI, DATETIME), by = "ID_CLI") %>%
  left_join(TIME_2 %>%
              select(ID_CLI, DATETIME), by = "ID_CLI") 

colnames(DATASET_1)<- c("ID_CLI", "GROSS_SALES","first", "last")

#JOIN BILLS
DATASET_2 <- DATASET_1 %>%
  left_join(BILLS %>%
              select(ID_CLI, TOT_BILLS), by = "ID_CLI") 

#importo lordo medio per scontrino
DATASET_3 <- DATASET_2 %>%
  mutate(AVG_BILL = as.numeric(GROSS_SALES/TOT_BILLS))

#JOIN DISCOUNT
DATASET_4 <- DATASET_3  %>%
  left_join(DISCOUNT %>%
              select(ID_CLI, DISC_TOT), by = "ID_CLI")

#ELIMINIAMO I DUPLICATI
DATASET_4 <- DATASET_4 %>% distinct(ID_CLI,  .keep_all = TRUE)

#ESTRAZIONE VARIABILE DISC_TAX = percentuale di sconto su importo lordo totale per cliente
DATASET_5 <- DATASET_4%>%
  mutate(DISC_TAX = as.numeric(DISC_TOT/GROSS_SALES))

#CORREGGIAMO EVENTUALI VALORI Inf
DATASET_5$DISC_TAX[which(!is.finite(DATASET_5$DISC_TAX))] <- 0

DATASET_6 <- DATASET_5 %>%
  left_join(ART_SOLD %>%
              select(ID_CLI, ART_SOLD), by = "ID_CLI") %>%
  mutate(AVG_ART = as.numeric(ART_SOLD/TOT_BILLS))

#ESTRAZIONE VARIABILE TARGET

#creazione variabile target nel dataset (per ora uguale a 0 per tutti i record)
DATASET_TARGET <- DATASET_6 %>%
  mutate(TARGET = as.factor(0))

#media <- TEMPO MEDIO DI RIACQUISTO = differnza di giorni fra ultima e prima data di acquisto diviso il numero di scontrini
DATASET_TARGET <- DATASET_TARGET %>%
  mutate(media = as.numeric(last-first)/TOT_BILLS)

#distribuzione di media
DISTR_RESELL <- boxplot(DATASET_TARGET[ ,"media"], main=DATASET_TARGET$media, col="lightblue", ylab=DATASET_TARGET$media) 

#individuazione treshold = mean + 2.5*sd 
treshold <- min(DISTR_RESELL$out)

#la soglia di giorni in cui un cliente è considerato fedele è uguale a 60 giorni dopo l'ultimo acquisto
#viene perciò individuata come data soglia il 28/02/2019, ovvero 60 giorni prima l'ultima data di rilevazioni (30/04/2019)
#scopo dell'analisi è individuare se un consumatore riacquisterà nella finestra di tempo 28/02/2019 - 30/04/2019
#se invece non effettuerà nessun acquisto nella finestra di tempo considerata, verrà individuato come CHURNER

soglia <- as.Date("2019-02-28")

#Escludiamo dall'analisi i clienti che hanno effettuato il primo acquisto dopo il 28/02, sui quali non siamo ancora in grado di determinare il comportamento
TARGET_CHURN <- DATASET_TARGET %>%
  filter(first < soglia) 

#CHURNER = 1
TARGET_A <- TARGET_CHURN %>%
  filter(last < soglia) %>%
  mutate(TARGET = 1)

#CLIENTI FEDELI = 0
TARGET_B <- TARGET_CHURN %>%
  filter(last >= soglia) %>%
  mutate(TARGET=0)

#UNIONE DATASET
TARGET_DEF <- rbind(TARGET_A,TARGET_B) %>%
  arrange(ID_CLI)

summary(TARGET_DEF$TARGET)

#ELIMINAZIONE VARIABILI NON UTILI AI FINI DELL'ANALISI
CHURN_DATASET <- TARGET_DEF[ ,-c(2,7,9)]

#Aggiungiamo una variabile geografica relativa alla macro area (Nord/Centro/Sud/Isole) dei customer
#estrazione regione di appartenenza
DATASET_A <- df_1_cli_fid_clean %>%
  left_join(df_2_cli_account_clean %>%
              select(ID_CLI, ID_ADDRESS), by = "ID_CLI") %>%
  left_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS, REGION), by = "ID_ADDRESS")

DATASET_CHURN <-  CHURN_DATASET %>%
  left_join(DATASET_A %>%
              select(ID_CLI, REGION), by = "ID_CLI")

#raggruppamento regioni in macro aree
REGION_NORD <- DATASET_CHURN %>%
  filter(REGION %in% c("VALLE D'AOSTA", "PIEMONTE", "LOMBARDIA", "LIGURIA", "FRIULI VENEZIA GIULIA", "VENETO", "TRENTINO ALTO ADIGE", "EMILIA ROMAGNA")) %>%
  mutate(REGION = "NORD")

REGION_CENTRO <- DATASET_CHURN %>%
  filter(REGION %in% c("TOSCANA", "MARCHE", "LAZIO", "UMBRIA")) %>%
  mutate(REGION = "CENTRO")

REGION_SUD <- DATASET_CHURN %>%
  filter(REGION %in% c("ABRUZZO", "MOLISE", "CAMPANIA", "BASILICATA", "PUGLIA", "CALABRIA")) %>%
  mutate(REGION = "SUD")

REGION_ISOLE <- DATASET_CHURN %>%
  filter(REGION %in% c("SICILIA", "SARDEGNA")) %>%
  mutate(REGION = "ISOLE")

#unione dataset
DATASET_CH <- rbind(REGION_NORD, REGION_CENTRO, REGION_SUD, REGION_ISOLE) %>%
  arrange(ID_CLI)
#8847 MISSIN REGION

#Aggiungiamo variabili circa il tipo di fidelizzazione del customer e il negozio di riferimento
DATASET_CH1 <- DATASET_CH %>%
  left_join(df_1_cli_fid_clean %>%
              select(ID_CLI, ID_NEG, COD_FID, STATUS_FID), by = "ID_CLI") %>%
  mutate(REGION = as.factor(REGION)) %>%
  mutate(TARGET = as.factor(TARGET)) 

DATASET_CH1 <- DATASET_CH1 %>%
  mutate(ID_CLI = as.factor(ID_CLI))

#standardizzazione variabili numeriche
DATASET_CH2 <- DATASET_CH1

ind <- sapply(DATASET_CH1, is.numeric)
DATASET_CH2[ind] <- lapply(DATASET_CH2[ind], scale)

#DATASET STANDARDIZZATO
DATASET_STD <- DATASET_CH2


library(cluster)
library(party)
library(Rtsne)
library(fpc)

# ONLY NUMERIC VARIABLES 

#si sviluppa un modello di clustering basato sulle variabili numeriche.
#l'aggiunta delle variabili qualitative avrebbe comportato una difficoltà computazionale molto elevata nel calcolo delle distanze di Gower, 
#costringedoci a limitare il dataset oggetto di analisi a solo il 10 % dei clienti

# K-MEANS #

DATASET_CLUST <- DATASET_STD[ ,c(4:7)]

#si valuta il numero k di clusters da creare fra 2 e 8
wss <- (nrow(DATASET_CLUST)-1)*sum(apply(DATASET_CLUST,2,var))
for (i in 2:8) wss[i] <- sum(kmeans(DATASET_CLUST, 
                                    centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit_kmeans <- kmeans(DATASET_STD[ ,c(4:7)], 4) # 4 cluster solution
# get cluster means 
fit_kmeans$centers
fit_kmeans$size


# append cluster assignment
mydata_kmeans <- data.frame(DATASET_CH1, fit_kmeans$cluster)

#medie dei valori non standardizzati per cluster
data_clusters <- mydata_kmeans %>%
  select(TOT_BILLS, AVG_BILL, DISC_TAX, AVG_ART, fit_kmeans.cluster)

CLUSTER_1 <- data_clusters %>%
  filter(fit_kmeans.cluster == 1) %>%
  mutate(TOT_BILLS = mean(TOT_BILLS)) %>%
  mutate(AVG_BILL = mean(AVG_BILL)) %>%
  mutate(DISC_TAX = mean(DISC_TAX)) %>%
  mutate(AVG_ART = mean(AVG_ART)) %>%
  filter(row_number() == 1) 

CLUSTER_2 <- data_clusters %>%
  filter(fit_kmeans.cluster == 2) %>%
  mutate(TOT_BILLS = mean(TOT_BILLS)) %>%
  mutate(AVG_BILL = mean(AVG_BILL)) %>%
  mutate(DISC_TAX = mean(DISC_TAX)) %>%
  mutate(AVG_ART = mean(AVG_ART)) %>%
  filter(row_number() == 1) 

CLUSTER_3 <- data_clusters %>%
  filter(fit_kmeans.cluster == 3) %>%
  mutate(TOT_BILLS = mean(TOT_BILLS)) %>%
  mutate(AVG_BILL = mean(AVG_BILL)) %>%
  mutate(DISC_TAX = mean(DISC_TAX)) %>%
  mutate(AVG_ART = mean(AVG_ART)) %>%
  filter(row_number() == 1) 

CLUSTER_4 <- data_clusters %>%
  filter(fit_kmeans.cluster == 4) %>%
  mutate(TOT_BILLS = mean(TOT_BILLS)) %>%
  mutate(AVG_BILL = mean(AVG_BILL)) %>%
  mutate(DISC_TAX = mean(DISC_TAX)) %>%
  mutate(AVG_ART = mean(AVG_ART)) %>%
  filter(row_number() == 1) 

#medie delle variabili per cluster
cluster_mean <- rbind(CLUSTER_1, CLUSTER_2, CLUSTER_3, CLUSTER_4)

#medie delle variabili nel dataset
pop_mean <- data_clusters %>%
  mutate(TOT_BILLS = mean(TOT_BILLS)) %>%
  mutate(AVG_BILL = mean(AVG_BILL)) %>%
  mutate(DISC_TAX = mean(DISC_TAX)) %>%
  mutate(AVG_ART = mean(AVG_ART)) %>%
  select(-fit_kmeans.cluster) %>%
  filter(row_number() == 1) 
  


#segmentazione deterministica geografica
GEO_means <- mydata_kmeans %>%
  select(TOT_BILLS, AVG_BILL, DISC_TAX, AVG_ART, REGION)

NORD_means <- GEO_means %>%
  filter(REGION == "NORD") %>%
  mutate(TOT_BILLS = mean(TOT_BILLS)) %>%
  mutate(AVG_BILL = mean(AVG_BILL)) %>%
  mutate(DISC_TAX = mean(DISC_TAX)) %>%
  mutate(AVG_ART = mean(AVG_ART)) %>%
  filter(row_number() == 1) 

CENTRO_means <- GEO_means %>%
  filter(REGION == "CENTRO") %>%
  mutate(TOT_BILLS = mean(TOT_BILLS)) %>%
  mutate(AVG_BILL = mean(AVG_BILL)) %>%
  mutate(DISC_TAX = mean(DISC_TAX)) %>%
  mutate(AVG_ART = mean(AVG_ART)) %>%
  filter(row_number() == 1) 

SUD_means <- GEO_means %>%
  filter(REGION == "SUD") %>%
  mutate(TOT_BILLS = mean(TOT_BILLS)) %>%
  mutate(AVG_BILL = mean(AVG_BILL)) %>%
  mutate(DISC_TAX = mean(DISC_TAX)) %>%
  mutate(AVG_ART = mean(AVG_ART)) %>%
  filter(row_number() == 1) 

ISOLE_means <- GEO_means %>%
  filter(REGION == "ISOLE") %>%
  mutate(TOT_BILLS = mean(TOT_BILLS)) %>%
  mutate(AVG_BILL = mean(AVG_BILL)) %>%
  mutate(DISC_TAX = mean(DISC_TAX)) %>%
  mutate(AVG_ART = mean(AVG_ART)) %>%
  filter(row_number() == 1) 

GEO <- rbind(NORD_means, CENTRO_means, SUD_means, ISOLE_means)
#Nessuna differenza particolare di prestazioni fra diverse aree geografiche



#-------------------------------
#-------------------------------
##### CHURN CLASSIFICATION #####
#-------------------------------
#-------------------------------

#DIVISIONE DATASET IN TRAINING E TEST
set.seed(12345)
training_ind <- DATASET_STD $TARGET%>% createDataPartition(p=0.6666, list = FALSE)

train_set <- DATASET_STD[training_ind, ] 
test_set <- DATASET_STD[-training_ind, ]

#TEST VARIABILI 

t.test(TOT_BILLS ~ TARGET, data = DATASET_STD )
t.test(AVG_BILL ~ TARGET, data = DATASET_STD )
t.test(DISC_TAX ~ TARGET, data = DATASET_STD )
t.test(AVG_ART ~ TARGET, data = DATASET_STD )

### ALGORITHMS ###

## LOGIT MODEL ##
logit_model<- glm(TARGET ~ TOT_BILLS + AVG_BILL + AVG_ART,
                  family = binomial(link = "logit"),
                  data = train_set)

summary(logit_model)
pander(anova(logit_model))

logit_predictions_prob <- plogis(predict(logit_model, test_set))

logit_predictions <- ifelse(logit_predictions_prob > 0.5, 1, 0)

logit_predictions <- as.factor(logit_predictions)
CONFUSION_MATRIX_CH_LOG <- confusionMatrix(logit_predictions, reference = test_set$TARGET, positive = "1")
CONFUSION_MATRIX_CH_LOG 

ROC_churn_logit <- plotROC(test_set$TARGET, logit_predictions_prob)


## TREE MODEL ##
library(tree)
contr <- tree.control(117587, mincut=30)
tree_model<- tree(TARGET ~ TOT_BILLS ,
                  data = train_set,
                  control = contr)

summary(tree_model)

tree_predictions_prob <- predict(tree_model, test_set)[ ,2]
#The function computes the optimal cutoff for the target variable with respect to misclassification
optCutOff <- optimalCutoff(test_set$TARGET, tree_predictions_prob)
soglia <- as.numeric(optCutOff)

tree_predictions <- ifelse(tree_predictions_prob > soglia, 1, 0)
tree_predictions <- as.factor(tree_predictions)

CONFUSION_MATRIX_CH_TREE <- confusionMatrix(tree_predictions, reference = test_set$TARGET,)
CONFUSION_MATRIX_CH_TREE

ROC_churn_tree <- plotROC(test_set$TARGET, tree_predictions_prob)

## ADVANCED ALGORITHMS

#CROSS VALIDATION
# define training control
set.seed(12345)
train_control <- trainControl(method="cv", number=5)

# fix the parameters of the algorithm
grid <- expand.grid(nIter=5)

# LOGISTIC BOOST
model_log_boost <- train(TARGET~TOT_BILLS + AVG_BILL + DISC_TAX + AVG_ART + REGION + ID_NEG + COD_FID + STATUS_FID, 
                         data=train_set, trControl=train_control, method="LogitBoost", tuneGrid = grid)

print(model_log_boost)

prob_log_boost <- predict(model_log_boost, newdata = test_set, type="prob")
prob_log_boost <-prob_log_boost[ ,2]

optCutOff_a <- optimalCutoff(test_set$TARGET, prob_log_boost)
pred_log_boost <- ifelse(prob_log_boost > optCutOff_a, 1, 0)

pred_log_boost <- as.factor(pred_log_boost)
CONFUSION_MATRIX_CH_LB <- confusionMatrix(pred_log_boost, test_set$TARGET)
CONFUSION_MATRIX_CH_LB
#accuracy poco minore rispetto al modello logistico semplice, ma i valori predetti sono più bilanciati (si ottimizzano congiuntamente specificy e sensitivity)

ROC_CH_LB <- plotROC(test_set$TARGET, prob_log_boost)



#ROC CURVES COMPARISON
preds_list <- list(tree_predictions_prob, logit_predictions_prob, prob_log_boost)

# List of actual values (same for all)
m <- length(preds_list)
actuals_list <- rep(list(test_set$TARGET), m)

library(ROCR)
# Plot the ROC curves
pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")

par(lwd= 4, lty= 6)
plot(rocs, col = as.list(1:m), main = "ROC Curves")
legend(x = "bottomright", 
       legend = c("Decision Tree", "Logit", "Logit_boost"),
       fill = 1:m)

