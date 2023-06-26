# Download data file
# download.file("https://github.com/rachelesrogers/ResponseType_Survey/raw/master/ResponseType.db", "data/ResponseType.db")



library(odbc)
library(dplyr)
library(tidyr)
library(stringr)

obs<-dbConnect(RSQLite::SQLite(),"data/ResponseType.db")
dbListTables(obs)
demo1<-dbGetQuery(obs, "SELECT * FROM demographics1")
results<-dbGetQuery(obs, "SELECT * FROM results")

results2 <- filter(results, !str_detect(prolificID, '[Tt]est|[Rr]achel|[Ss]usan')) %>%
  mutate(strength = str_remove(strength, " .*$")) %>%
  mutate(fixed_like = factor(fixed_like, levels = c(
    "Impossible that he is guilty",
    "About 1 chance in 10,000",
    "About 1 chance in 1,000",
    "About 1 chance in 100",
    "About 1 chance in 10",
    "1 chance in 2 (fifty-fifty chance)",
    "About 9 chances in 10",
    "About 99 chances in 100",
    "About 999 chances in 1,000",
    "About 9,999 chances in 10,000",
    "Certain to be guilty"), labels = c(
      "Innocent",
      "1/10,000",
      "1/1,000",
      "1/100",
      "1/10",
      "1/2",
      "9/10",
      "99/100",
      "999/1,000",
      "9,999/10,000",
      "Guilty"), ordered = T),
    guilty = factor(guilty, levels = c("No", "Yes"), labels = c("Vote:\nNot Guilty", "Vote:\nGuilty")),
    opinion_guilt = factor(opinion_guilt, levels = c("No", "Yes"), labels = c("Opinion:\nNot Guilty", "Opinion:\nGuilty")),
    # strength = factor(strength, levels = as.character(1:9), labels = c("Not at all\nstrong", "2", "3","4", "Moderately\nstrong", "6","7","8","Extremely\nstrong")),
    conclusion = factor(conclusion, c("NoMatch", "Match"), ordered = T),
    prob_vis_fact = factor(sprintf("%03d", prob_vis)),
    prob_hide_fact = factor(sprintf("%03d", prob_hide)),
    guilt_calc_num = ifelse(is.na(guilt_free_num), innocent_free_denom - innocent_free_num, guilt_free_num),
    guilt_calc_denom = ifelse(is.na(guilt_free_denom), innocent_free_denom, guilt_free_denom),
    guilt_calc_prop = (guilt_calc_num)/(guilt_calc_denom),
    conclusion_nice = factor(conclusion, levels = c("NoMatch", "Match"), labels = c("Evidence:\nNo Match", "Evidence:\nMatch"))
  ) |>
  mutate(logGuiltCalc = log10(guilt_calc_num) - log10(guilt_calc_denom))
