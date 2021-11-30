#install packages
# install.packages("tableone")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("remotes")
# remotes::install_github("Lingtax/misinformation")
# install.packages("psych")

# specify packages ---------------------------------------------
library(misinformation)
library(tidyverse)
library(tableone)
library(dplyr)
library(psych)
library(tidyr)
library(lme4)
library(lmerTest)

#get the data  ------------------------------------------------
df <-read_qualtrics("2020_ta_fake_data.csv")
df <-read_csv(here::here("simulated","2020_ta_fake_data.csv"))
#metadata rename
meta <- read_csv("metadata_2020_thinkagain.csv")
df <-  meta_rename(df, meta, old = name_raw, new = name_clean)

# data cleaning -----------------------------------------------
#reverse coding, summing measures
df <- df %>% mutate(cc2_prer = 8 - cc2_pre,
                    cc_pre = cc1_pre + cc2_prer + cc3_pre,
                    vax1_prer = 8 - vax1_pre,
                    vax_pre = vax1_prer + vax2_pre + vax3_pre,
                    cam3_prer = 8 - cam3_pre,
                    cam_pre = cam1_pre + cam2_pre + cam3_prer,
                    cc2_postr = 8 - cc2_post,
                    cc_post = cc1_post + cc2_postr + cc3_post,
                    vax1_postr = 8 - vax1_post,
                    vax_post = vax1_postr + vax2_post + vax3_post,
                    cam3_postr = 8 - cam3_post,
                    cam_post = cam1_post + cam2_post + cam3_postr, 
                    cc_misinform = ifelse(exposure == "climate", TRUE, FALSE),
                    vax_misinform = ifelse(exposure == "vax", TRUE, FALSE),
                    cam_misinform = ifelse(exposure == "cam", TRUE, FALSE)
                    )
# descriptives  -----------------------------------------------
CreateTableOne(vars = c('age', 'gender', 'education', 'cc_pre',
                        'cc_post', 'cam_pre', 'cam_post', 'vax_pre', 'vax_post'),
               strata= c("condition", "cc_misinform"), 
               factorVars = c('gender', 'education'), data=df )

# reliabilities -----------------------------------------------------------

df %>% select(cc1_pre, cc2_prer, cc3_pre) %>% omega()
df %>% select(cc1_post, cc2_post, cc3_post) %>% omega()
df %>% select (cam1_pre, cam2_pre, cam3_prer) %>% omega()

# Data restructure --------------------------------------------------------

df_tidy <- df %>% 
  select(response_id, age, gender, education, ends_with("misinform"), 
         condition, ends_with("pre"), ends_with("post"), 
         -starts_with("tech"), -starts_with("diet"), 
         -starts_with("rights"), -matches("[0-9]")) %>% 
  pivot_longer(cols = cc_pre:cam_post, 
               names_to = c("outcome", "time"), 
               names_pattern = "(.+)_(.+)")

cam_tidy <- df_tidy %>% 
  filter(outcome == "cam") %>% 
  select(-starts_with("cc_"), -starts_with("vax_"))
cc_tidy <- df_tidy %>% 
  filter(outcome == "cc") %>% 
  select(-starts_with("cam_"), -starts_with("vax_"))
vax_tidy <- df_tidy %>% 
  filter(outcome == "vax") %>% 
  select(-starts_with("cc_"), -starts_with("cam_"))


cam_mlmod <- lmer(value ~ cam_misinform*condition*time +(1|response_id), data = cam_tidy)
cc_mlmod <- lmer(value ~ cc_misinform*condition*time +(1|response_id), data = cc_tidy)
vax_mlmod <- lmer(value ~ vax_misinform*condition*time +(1|response_id), data = vax_tidy)


cam_tidy %>% ggplot(aes(x = cam_misinform, y = value, fill = condition)) + 
  geom_violin(alpha = 0.5) + 
  geom_jitter()  +
  theme_classic() + 
  labs(x= "Misinformed", y = "Belief", fill = "Condition")
