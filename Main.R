# Load required libs
library(config)
library(ggplot2)
library(scales)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)

# Create dirs (dir.create() does not crash when dir already exists)
dir.create(here::here("Data"), showWarnings = FALSE)
dir.create(here::here("Reports"), showWarnings = FALSE)
dir.create(here::here("SQL"), showWarnings = FALSE)

# Create constants
# Set threshold for error relative frequency
freq_lim = 0.02

#########################################################################################
# Data Extraction #######################################################################
#########################################################################################

# Set JAVA_HOME, set max. memory, and load rJava library
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jre1.8.0_171")
options(java.parameters = "-Xmx2g")
library(rJava)

# Output Java version
.jinit()
print(.jcall("java/lang/System", "S", "getProperty", "java.version"))

# Load RJDBC library
library(RJDBC)

# Get credentials
ablak <-
  config::get("ablak" , file = "C:\\Users\\PoorJ\\Projects\\config.yml")

# Create connection driver
jdbcDriver <-
  JDBC(driverClass = "oracle.jdbc.OracleDriver", classPath = "C:\\Users\\PoorJ\\Desktop\\ojdbc7.jar")


# Open connection: ablak ----------------------------------------------------------------
jdbcConnection <-
  dbConnect(
    jdbcDriver,
    url = ablak$server,
    user = ablak$uid,
    password = ablak$pwd
  )

# Fetch data
readQuery <-
  function(file)
    paste(readLines(file, warn = FALSE), collapse = "\n")

query_error_freq <-
  readQuery(here::here("SQL", "autouw_live_error_freq.sql"))
query_error_pattern <- "select * from t_kpm_live_pattern"
query_autouw <- "select * from t_erk_kpm_live"
query_autouw_dict <- "select * from t_autouw_dict"

autouw <- dbGetQuery(jdbcConnection, query_autouw)
autouw_dict <- dbGetQuery(jdbcConnection, query_autouw_dict)
autouw_error_freq <- dbGetQuery(jdbcConnection, query_error_freq)
autouw_error_pattern <-
  dbGetQuery(jdbcConnection, query_error_pattern)

# Close db connection: ablak
dbDisconnect(jdbcConnection)



#########################################################################################
# Analyze Attempt & Success Rates on a Monthly Basis ####################################
#########################################################################################

# Transformations
autouw_main <- autouw[!is.na(autouw$MODTYP), ]
autouw_main$IDOSZAK <-
  paste0(substr(autouw_main$IDOSZAK, 1, 4), "/", substr((autouw_main$IDOSZAK), 6, 7))

autouw_main <-  autouw_main %>%
  mutate(MODTYP = case_when(.$MODTYP == "Vagyon" ~ "Lakás",
                            TRUE ~ .$MODTYP))


# Compute autoUW main KPIs for Home------------------------------------------------------
# Compute attempt rate
home_attempt <-  autouw_main %>%
  filter(MODTYP == "Lakás") %>% 
  mutate(ATTEMPT = case_when(.$KPM %in% c("Sikeres", "Sikertelen") ~ 'I',
                             TRUE ~ 'N')) %>%
  group_by(IDOSZAK, ATTEMPT) %>%
  summarise (TOTAL = n()) %>%
  mutate(KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(ATTEMPT == "I") %>%
  select(IDOSZAK, KISERELT)


# Compute success rate within total
home_success_total <-  autouw_main %>%
  filter(MODTYP == "Lakás") %>% 
  group_by(IDOSZAK, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_TELJES = TOTAL / sum(TOTAL)) %>%
  filter(KPM == "Sikeres") %>%
  select(IDOSZAK, SIKER_PER_TELJES)


# Compute success rate within attempted
home_success_attempt <-  autouw_main %>%
  filter(MODTYP == "Lakás") %>% 
  filter(KPM != "Nincs") %>%
  group_by(IDOSZAK, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(KPM == "Sikeres") %>%
  select(IDOSZAK, SIKER_PER_KISERELT)


# Merge KPI results
home_main <- home_attempt %>%
  left_join(home_success_attempt, by = "IDOSZAK") %>%
  left_join(home_success_total, by = "IDOSZAK") %>%
  gather(MUTATO, PCT,-IDOSZAK) %>% 
  ungroup()


# Get previous months
home_hist <- read.csv(here::here("Data", "p2_auw_prod.csv"), stringsAsFactors = FALSE) %>% 
                filter(MODTYP == "Lakás") %>% 
                select(-MODTYP)

home_main <- rbind(home_hist, home_main) %>% 
                arrange(MUTATO, IDOSZAK)

# Save for dashboard output
write.csv(home_main,
          here::here("Data", "home_main.csv"),
          row.names = FALSE)


# Compute autoUW main KPIs for Home per Product -----------------------------------------
# Compute attempt rate
home_prod_attempt <-  autouw_main %>%
  filter(MODTYP == "Lakás" & MODKOD %in% c("21968", "21972")) %>% 
  mutate(ATTEMPT = case_when(.$KPM %in% c("Sikeres", "Sikertelen") ~ 'I',
                             TRUE ~ 'N')) %>%
  group_by(MODKOD, ATTEMPT) %>%
  summarise (TOTAL = n()) %>%
  mutate(KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(ATTEMPT == "I") %>%
  select(MODKOD, KISERELT)


# Compute success rate within total
home_prod_success_total <-  autouw_main %>%
  filter(MODTYP == "Lakás" & MODKOD %in% c("21968", "21972")) %>% 
  group_by(MODKOD, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_TELJES = TOTAL / sum(TOTAL)) %>%
  filter(KPM == "Sikeres") %>%
  select(MODKOD, SIKER_PER_TELJES)


# Compute success rate within attempted
home_prod_success_attempt <-  autouw_main %>%
  filter(MODTYP == "Lakás" & MODKOD %in% c("21968", "21972")) %>% 
  filter(KPM != "Nincs") %>%
  group_by(MODKOD, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(KPM == "Sikeres") %>%
  select(MODKOD, SIKER_PER_KISERELT)


# Merge KPI results
home_prod_main <- home_prod_attempt %>%
  left_join(home_prod_success_attempt, by = "MODKOD") %>%
  left_join(home_prod_success_total, by = "MODKOD") %>%
  gather(MUTATO, PCT,-MODKOD) %>% 
  ungroup()


# Save for dashboard output
write.csv(home_prod_main,
          here::here("Data", "home_prod_main.csv"),
          row.names = FALSE)


#########################################################################################
# Analyze Error Type Frequencies  #######################################################
#########################################################################################

# Transformations
# Extract freqs from last 3 months to get valid understanding of present root-causes
autouw_error_freq <-
  autouw_error_freq[!is.na(autouw_error_freq$MODTYP), ]

autouw_error_freq <-  autouw_error_freq %>%
  mutate(MODTYP = case_when(.$MODTYP == "Vagyon" ~ "Lakás",
                            TRUE ~ .$MODTYP)) %>%
  left_join(autouw_dict, by = c("HIBAAZON"))

autouw_error_freq_last3 <-
  autouw_error_freq %>% filter(ymd_hms(IDOSZAK) >= floor_date(Sys.Date(), unit = "month") - months(3))

autouw_error_freq_last3$IDOSZAK <-
  paste0(substr(autouw_error_freq_last3$IDOSZAK, 1, 4),
         "/",
         substr((autouw_error_freq_last3$IDOSZAK), 6, 7))

autouw_error_freq$IDOSZAK <-
  paste0(substr(autouw_error_freq$IDOSZAK, 1, 4), "/", substr((autouw_error_freq$IDOSZAK), 6, 7))


# Freq of errors for whole dataset
freq <- autouw_error_freq_last3 %>%
  group_by(HIBAAZON, HIBA) %>%
  summarize(TOTAL = n()) %>%
  ungroup() %>%
  arrange(desc(TOTAL)) %>%
  mutate(GYAKORISAG = TOTAL / sum(TOTAL),
         GYAK_KUM = cumsum(GYAKORISAG)) %>%
  filter(GYAKORISAG >= freq_lim)


# Save for dashboard output
write.csv(freq,
          here::here("Data", "p3_error_freq.csv"),
          row.names = FALSE)


# Freq of errors per prod line
freq_prod <- autouw_error_freq_last3 %>%
  group_by(MODTYP, HIBAAZON, HIBA) %>%
  summarize(TOTAL = n()) %>%
  arrange(MODTYP, desc(TOTAL)) %>%
  ungroup() %>%
  group_by(MODTYP) %>%
  mutate(GYAKORISAG = TOTAL / sum(TOTAL)) %>%
  filter(GYAKORISAG >= freq_lim)


# Save for dashboard output
write.csv(freq_prod,
          here::here("Data", "p3_error_freq_prod.csv"),
          row.names = FALSE)


# Freq time series for most common errors (of last 3 months) per prod line
# Most common errors extracted from last 3 months then track them backwards in time
most_common <- unique(freq_prod$HIBAAZON)
autouw_error_freq_mc <- autouw_error_freq %>%
  filter(HIBAAZON %in% most_common)

freq_prod_mc <- autouw_error_freq_mc %>%
  group_by(IDOSZAK, MODTYP, HIBAAZON, HIBA) %>%
  summarize(TOTAL = n()) %>%
  arrange(IDOSZAK, MODTYP, desc(TOTAL)) %>%
  ungroup() %>%
  group_by(IDOSZAK, MODTYP) %>%
  mutate(GYAKORISAG = TOTAL / sum(TOTAL))


# Save for dashboard output
write.csv(freq_prod_mc,
          here::here("Data", "p3_error_freq_prod_mc.csv"),
          row.names = FALSE)



#########################################################################################
# Analyze Error Pattern Frequencies  ####################################################
#########################################################################################

# Transformations
# Extract patterns from last 3 months to get valid understanding of present root-causes
autouw_error_pattern <-
  autouw_error_pattern[!is.na(autouw_error_pattern$MODTYP),]

autouw_error_pattern <-  autouw_error_pattern %>%
  mutate(MODTYP = case_when(.$MODTYP == "Vagyon" ~ "Lakás",
                            TRUE ~ .$MODTYP))

autouw_error_pattern_last3 <-
  autouw_error_pattern %>% filter(ymd_hms(IDOSZAK) >= floor_date(Sys.Date(), unit = "month") - months(3))

autouw_error_pattern$IDOSZAK <-
  paste0(substr(autouw_error_pattern$IDOSZAK, 1, 4), "/", substr((autouw_error_pattern$IDOSZAK), 6, 7))

autouw_error_pattern_last3$IDOSZAK <-
  paste0(substr(autouw_error_pattern_last3$IDOSZAK, 1, 4),
         "/",
         substr((autouw_error_pattern_last3$IDOSZAK), 6, 7))


# Freq of patterns for whole dataset
freq_pattern <- autouw_error_pattern_last3 %>%
  group_by(HIBA_MINTA) %>%
  summarize(TOTAL = n()) %>%
  ungroup() %>%
  arrange(desc(TOTAL)) %>%
  mutate(GYAKORISAG = TOTAL / sum(TOTAL),
         GYAK_KUM = cumsum(GYAKORISAG)) %>%
  filter(GYAKORISAG >= freq_lim)


# Save for dashboard output
write.csv(freq_pattern,
          here::here("Data", "p4_error_freq_pattern.csv"),
          row.names = FALSE)


# Freq of patterns per prod line
freq_pattern_prod <- autouw_error_pattern_last3 %>%
  group_by(MODTYP, HIBA_MINTA) %>%
  summarize(TOTAL = n()) %>%
  arrange(MODTYP, desc(TOTAL)) %>%
  ungroup() %>%
  group_by(MODTYP) %>%
  mutate(GYAKORISAG = TOTAL / sum(TOTAL)) %>%
  filter(GYAKORISAG >= freq_lim)


# Save for dashboard output
write.csv(
  freq_pattern_prod,
  here::here("Data", "p4_error_freq_pattern_prod.csv"),
  row.names = FALSE
)


# Freq time series for most common patterns per prod line
most_common_pattern <- unique(freq_pattern_prod$HIBA_MINTA)
autouw_error_pattern_mc <- autouw_error_pattern %>%
  filter(HIBA_MINTA %in% most_common_pattern)

error_prod_mc <- autouw_error_pattern_mc %>%
  group_by(IDOSZAK, MODTYP, HIBA_MINTA) %>%
  summarize(TOTAL = n()) %>%
  arrange(IDOSZAK, MODTYP, desc(TOTAL)) %>%
  ungroup() %>%
  group_by(IDOSZAK, MODTYP) %>%
  mutate(GYAKORISAG = TOTAL / sum(TOTAL))


# Save for dashboard output
write.csv(
  error_prod_mc,
  here::here("Data", "p4_error_freq_pattern_prod_mc.csv"),
  row.names = FALSE
)



#########################################################################################
# Analyze Error Pattern Costs  ##########################################################
#########################################################################################

# Transformations
autouw_cost <-
  autouw_cost[!is.na(autouw_cost$MODTYP),]

autouw_cost <-  autouw_cost %>%
  mutate(MODTYP = case_when(.$MODTYP == "Vagyon" ~ "Lakás",
                            TRUE ~ .$MODTYP))


# Compute monthly total cost -----------------------------------------------------------
cost_monthly <- autouw_cost %>%
  group_by(IDOSZAK) %>%
  summarize(HIBA_IDO = sum(IDO_PERC)) %>%
  ungroup() %>%
  left_join(num_wdays, by = c("IDOSZAK")) %>%
  mutate(FTE = HIBA_IDO / 60 / 7 / MNAP,
         IDOSZAK = paste0(substr(IDOSZAK, 1, 4), "/", substr((IDOSZAK), 6, 7))) %>%
  left_join(auw_main[auw_main$MUTATO == "SIKER_PER_TELJES",], by = c("IDOSZAK")) %>%
  rename(SIKER_PER_TELJES = PCT) %>%
  select(IDOSZAK, FTE, SIKER_PER_TELJES)


# Save for dashboard output
write.csv(cost_monthly,
          here::here("Data", "p1_cost_monthly.csv"),
          row.names = FALSE)


# Compute monthly total cost per prod line ----------------------------------------------
cost_prod_monthly <- autouw_cost %>%
  group_by(IDOSZAK, MODTYP) %>%
  summarize(HIBA_IDO = sum(IDO_PERC)) %>%
  ungroup() %>%
  left_join(num_wdays, by = c("IDOSZAK")) %>%
  mutate(FTE = HIBA_IDO / 60 / 7 / MNAP,
         IDOSZAK = paste0(substr(IDOSZAK, 1, 4), "/", substr((IDOSZAK), 6, 7))) %>%
  left_join(auw_prod[auw_prod$MUTATO == "SIKER_PER_TELJES",], by = c("IDOSZAK", "MODTYP")) %>%
  rename(SIKER_PER_TELJES = PCT) %>%
  select(IDOSZAK, MODTYP, FTE, SIKER_PER_TELJES)


# Save for dashboard output
write.csv(cost_prod_monthly,
          here::here("Data", "p5_cost_prod_monthly.csv"),
          row.names = FALSE)



# Compute monthly total cost per error pattern ------------------------------------------
# NOTE: normalized FTE computed for last 3 months: must make sure both FTE and N are computed
# for 3 months period
wdays_last3 <-
  num_wdays %>% filter(
    ymd_hms(IDOSZAK) >= floor_date(Sys.Date(), unit = "month") - months(3) &
      ymd_hms(IDOSZAK) < floor_date(Sys.Date(), unit = "month")
  ) %>%
  select(MNAP) %>% sum

fte_last3 <- autouw_cost %>%
  filter(ymd_hms(IDOSZAK) >= floor_date(Sys.Date(), unit = "month") - months(3)) %>%
  group_by(MODTYP, HIBA_MINTA) %>%
  summarize(HIBA_IDO = sum(IDO_PERC)) %>%
  ungroup() %>%
  mutate(FTE = HIBA_IDO / 60 / 7 / wdays_last3)

cost_pattern_last3 <-
  freq_pattern_prod %>% left_join(fte_last3, by = c("MODTYP", "HIBA_MINTA")) %>%
  mutate(FAJL_FTE = FTE / TOTAL * 1000) %>%
  select(MODTYP, HIBA_MINTA, GYAKORISAG, FTE, FAJL_FTE) %>%
  arrange(MODTYP, desc(FTE))


# Save for dashboard output
write.csv(cost_pattern_last3,
          here::here("Data", "p5_cost_pattern_last3.csv"),
          row.names = FALSE)
