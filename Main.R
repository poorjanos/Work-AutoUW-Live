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
freq_lim = 0.01

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



# Compute autoUW main KPIs for TPML---------------------------------------------------
# Compute attempt rate
tpml_attempt <-  autouw_main %>%
  filter(MODTYP == "GFB") %>%
  filter(ymd_hms(ERKDAT) < Sys.Date() - 5) %>% 
  mutate(ATTEMPT = case_when(.$KPM %in% c("Sikeres", "Sikertelen") ~ 'I',
                             TRUE ~ 'N')) %>%
  group_by(IDOSZAK, ATTEMPT) %>%
  summarise (TOTAL = n()) %>%
  mutate(KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(ATTEMPT == "I") %>%
  select(IDOSZAK, KISERELT)


# Compute success rate within total
tpml_success_total <-  autouw_main %>%
  filter(MODTYP == "GFB") %>% 
  filter(ymd_hms(ERKDAT) < Sys.Date() - 5) %>% 
  group_by(IDOSZAK, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_TELJES = TOTAL / sum(TOTAL)) %>%
  filter(KPM == "Sikeres") %>%
  select(IDOSZAK, SIKER_PER_TELJES)


# Compute success rate within attempted
tpml_success_attempt <-  autouw_main %>%
  filter(MODTYP == "GFB") %>% 
  filter(ymd_hms(ERKDAT) < Sys.Date() - 5) %>% 
  filter(KPM != "Nincs") %>%
  group_by(IDOSZAK, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(KPM == "Sikeres") %>%
  select(IDOSZAK, SIKER_PER_KISERELT)


# Merge KPI results
tpml_main <- tpml_attempt %>%
  left_join(tpml_success_attempt, by = "IDOSZAK") %>%
  left_join(tpml_success_total, by = "IDOSZAK") %>%
  gather(MUTATO, PCT,-IDOSZAK) %>% 
  ungroup()


# Get previous months
tpml_hist <- read.csv(here::here("Data", "p2_auw_prod.csv"), stringsAsFactors = FALSE) %>% 
                filter(MODTYP == "GFB") %>% 
                select(-MODTYP)

tpml_main <- rbind(tpml_hist, tpml_main) %>% 
                arrange(MUTATO, IDOSZAK)

# Save for dashboard output
write.csv(tpml_main,
          here::here("Data", "tpml_main.csv"),
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

autouw_error_freq$IDOSZAK <-
  paste0(substr(autouw_error_freq$IDOSZAK, 1, 4),
         "/",
         substr((autouw_error_freq$IDOSZAK), 6, 7))


# Freq of errors per Home product
home_prod_error_freq <- autouw_error_freq %>%
  filter(MODTYP == "Lakás" & MODKOD %in% c("21968", "21972")) %>% 
  group_by(MODKOD, HIBAAZON, HIBA) %>%
  summarize(TOTAL = n()) %>%
  arrange(MODKOD, desc(TOTAL)) %>%
  ungroup() %>%
  group_by(MODKOD) %>%
  mutate(GYAKORISAG = TOTAL / sum(TOTAL)) %>%
  filter(GYAKORISAG >= freq_lim)


# Save for dashboard output
write.csv(home_prod_error_freq,
          here::here("Data", "home_prod_error_freq.csv"),
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

autouw_error_pattern$IDOSZAK <-
  paste0(substr(autouw_error_pattern$IDOSZAK, 1, 4), "/", substr((autouw_error_pattern$IDOSZAK), 6, 7))


# Freq of patterns per Home prods
home_freq_pattern_prod <- autouw_error_pattern %>%
  filter(MODTYP == "Lakás" & MODKOD %in% c("21968", "21972")) %>% 
  group_by(MODKOD, HIBA_MINTA) %>%
  summarize(TOTAL = n()) %>%
  arrange(MODKOD, desc(TOTAL)) %>%
  ungroup() %>%
  group_by(MODKOD) %>%
  mutate(GYAKORISAG = TOTAL / sum(TOTAL)) %>%
  filter(GYAKORISAG >= freq_lim)


# Save for dashboard output
write.csv(
  home_freq_pattern_prod,
  here::here("Data", "home_prod_freq_pattern.csv"),
  row.names = FALSE
)