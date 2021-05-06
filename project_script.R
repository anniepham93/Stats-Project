###Getting Twitter data for sentiment analysis
key <- "3F7sTO0f5cEfCvytuQ4G9PXhz"
secret <- "0B8c0np7JSph35RluBDz62u9rOvajYwyDybf9ukvFiEvQyf1aZ"
access_token <- "394966290-A20uxv65Gk6huZrobLXXQYQcEE1rLtBAvhvJDxu7"
access_secret <- "vElMOcaHGDPGPOu8NzrTdYoRr88YlVmguUZnBur1bAXkn"
twitter_token <- create_token(
app = appname,
consumer_key = key,
consumer_secret = secret,
access_token = access_token,
access_secret = access_secret)
gamergate_data <- search_fullarchive("#gamergate", n=5000, env_name="gamergate", fromDate="201408010000", toDate="201512310000")
gamergate_data <- gamergate_data[,-c(1:4, 6:16, 18:90)]
gamergate_data <- gamergate_data[, -2]
write.csv(gamergate_data, "gamergate_data.csv")

### Cleaning the Data 
### The data was taken from Pew Research Survey, waves 4, 24, and 32

### Wave 4 cleaning 
ATP_W4 <- read_sav("wave4-2014/W4_Jun14/ATP W4.sav")
ATP_W4 <- ATP_W4[, -c(2:27, 37:158, 203, 207, 210, 211:224)]
ATP_W4 <- ATP_W4[, -59]
ATP_W4 <- ATP_W4[, c(55:58, 1:54)]
ATP_W4 <- ATP_W4[, c(5, 1:4, 6:58)]
ATP_W4 <- ATP_W4[, -c(2, 4)]
write.csv(ATP_W4, "ATP_W4.csv")
ATP_W4 <- ATP_W4[, -4]
ATP_W4_clean <- ATP_W4_clean[, -c(12:18)]
ATP_W4_clean <- ATP_W4_clean[, -c(27:34)]
ATP_W4_clean <- ATP_W4_clean %>% rename(sex=F_SEX_FINAL, race=F_RACECMB_TYPOLOGY, 
                                        offensive_name=ON7_A_W4, physical_threat=ON7_B_W4, 
                                        harass_over_time=ON7_C_W4, stalk=ON7_D_W4, 
                                        embarrass=ON7_E_W4, sexual_harass=ON7_H_W4, 
                                        harass_count=ON7_YES_W4, harass_period=ON9_W4, 
                                        upset_scale=ON10_W4, harass_social_network=ON12_A_W4, 
                                        harass_online_dating=ON12_B_W4, 
                                        harass_website_comments=ON12_D_W4, 
                                        harass_gaming=ON12_E_W4, 
                                        harass_email=ON12_F_W4, harass_forum=ON12_G_W4, 
                                        respond_vs_ignore=ON14_W4, withdrew_online=ON15_A_W4, 
                                        unfriend=ON15_B_W4, confront=ON15_C_W4, 
                                        change_username=ON15_D_W4, discuss=ON15_F_W4, 
                                        report_web=ON15_G_W4, stop_attending_event=ON15_H_W4, 
                                        report_police=ON15_I_W4, other=ON15_J_W4, 
                                        action_effective=ON16_W4, ignore_effective=ON17_W4, 
                                        reputation=ON18_W4)
ATP_W4$F_SEX_FINAL[ATP_W4$F_SEX_FINAL == "2"] <- "F"
ATP_W4$F_SEX_FINAL[ATP_W4$F_SEX_FINAL == 2,] <- "F"
ATP_W4$F_SEX_FINAL[ATP_W4$F_SEX_FINAL == "2",] <- "F"
ATP_W4$F_SEX_FINAL <- as.character(ATP_W4$F_SEX_FINAL)
ATP_W4$F_SEX_FINAL[ATP_W4$F_SEX_FINAL == "2",] <- "F"
ATP_W4$F_SEX_FINAL[ATP_W4$F_SEX_FINAL == "2"] <- "F"
ATP_W4_clean$sex <- as.character(ATP_W4_clean$sex)
ATP_W4_clean$sex[ATP_W4_clean$sex=="2"] <- "F"
ATP_W4_clean$sex[ATP_W4_clean$sex=="1"] <- "M"
ATP_W4_clean$sex[ATP_W4_clean$sex=="99"] <- "NA"
ATP_W4_clean$race <- as.character(ATP_W4_clean$race)
ATP_W4_clean$race[ATP_W4_clean$race=="1"] <- "White"
ATP_W4_clean$race[ATP_W4_clean$race=="2"] <- "Black"
ATP_W4_clean$race[ATP_W4_clean$race=="3"] <- "Asian"
ATP_W4_clean$race[ATP_W4_clean$race=="4"] <- "Mixed"
ATP_W4_clean$race[ATP_W4_clean$race=="5"] <- "Other"
ATP_W4_clean$race[ATP_W4_clean$race=="9"] <- "NA"
write.csv(ATP_W4_clean, "ATP_W4_clean.csv")
ATP_W4_clean <- read_csv("ATP_W4_clean.csv")
ATP_W4_clean <- ATP_W4_clean[, -1]
ATP_W4_clean <- ATP_W4_clean[, -c(4:18, 20:39)]

### Wave 24 cleaning 
ATP_W24 <- read_sav("wave24-2017/W24_Jan17/ATP W24.sav")
ATP_W24 <- ATP_W24[, -c(2:14, 81:106, 157:164, 180:187, 190:236)]
ATP_W24 <- ATP_W24[, -c(32:59)]
ATP_W24 <- ATP_W24[, -c(10, 18:30, 47:77)]
ATP_W24 <- ATP_W24 %>% rename(kindness_online=KIND1_W24, problem_harass=PROB1A_W24, 
                              problem_privacy=PROB1B_W24, problem_false_info=PROB1C_W24, 
                              problem_time_waste=PROB1D_W24)
ATP_W24 <- ATP_W24[, -c(7:9)]
ATP_W24 <- ATP_W24 %>% rename(welcome_socialnetwork=ON4A_W24, welcome_online_dating=ON4B_W24, 
                              welcome_website_comments=ON4D_W24, welcome_online_gaming=ON4E_W24, 
                              welcome_discussion_forum=ON4F_W24, offensive_content=JOKE1_W24, 
                              online_freedom=CHOICE1_W24, untrue_post=UNTRUE1_W24, 
                              experience_doxxing=EXP2_A_W24, experience_swatting=EXP2_B_W24, 
                              experience_hacking=EXP2_C_W24, experience_trolling=EXP2_D_W24, 
                              experience_none=EXP2_E_W24, non_consent_photo_post=EXP3A_W24, 
                              non_consent_photo_sent=EXP3B_W24, personal_offensive_names=ON7_A_W24, 
                              personal_physical_threat=ON7_B_W24, personal_harass_over_time=ON7_C_W24, 
                              personal_stalk=ON7_D_W24, personal_embarrass=ON7_E_W24, 
                              personal_sexual_harass=ON7_H_W24, harass_count=ON7_YES_W24, 
                              upset_scale=ON10_W24, multiple_platform=MULT1_W24, 
                              env_social_network=ON12_A_W24, env_online_dating=ON12_B_W24, 
                              env_website_comments=ON12_D_W24, env_online_gaming=ON12_E_W24, 
                              env_email=ON12_F_W24, env_discussion_forum=ON12_G_W24, 
                              env_text=ON12_H_W24, env_other_app=ON12_I_W24, multi_people=MULT2_W24)
ATP_W24 <- ATP_W24[, -c(20, 40, 42:56)]
ATP_W24 <- ATP_W24 %>% rename(still_consider_harassment=YOU1_W24, danger_risk=DANGER1_W24)

### Wave 32 Cleaning 
ATP_W32 <- read_sav("wave32-2018/W32_Feb18/ATP W32.sav")
ATP_W32 <- ATP_W32[, -c(2:54, 63:110, 112:145)]
ATP_W32 <- ATP_W32[, -c(2:5)]
ATP_W32 <- ATP_W32 %>% rename(increase_focus_harassment_opportunities=HARASS2F1_W32, 
                              increase_focus_harassment_interact=HARASS3F2_W32, 
                              harass_experience=HARASS4_W32, harass_setting=HARASS5_W32, 
                              women_obstacles=WOMENOPPS_W32)
write.csv(ATP_W32, "ATP_32.csv")

### Dataset 
ATP <- merge(ATP_W4_clean, ATP_W32, by="QKEY")
ATP <- merge(ATP, ATP_W24, by="QKEY")

###Frequency by Race:
frequency_by_race <- ggplot(ATP_v2, aes(y=sex, x=race, color=race, group=sex)) 
+ geom_count(alpha=0.5) + labs(title="Frequency by Race", x="Race", y="Sex", size="")
ggplotly(frequency_by_race)


### Analysis Time!
### Use Chi Square test of independence to determine what variables to focus on 
chisq.test(ATP$sex, ATP$harass_period) #delete
chisq.test(ATP$sex, ATP$reputation) #delete
chisq.test(ATP$sex, ATP$increase_focus_harassment_opportunities) #delete
chisq.test(ATP$sex, ATP$increase_focus_harassment_interact)
ATP_v2 <- ATP[, -c(4:6)]
chisq.test(ATP_v2$sex, ATP_v2$harass_experience)
chisq.test(ATP_v2$sex, ATP_v2$harass_setting)
chisq.test(ATP_v2$sex, ATP_v2$women_obstacles)
chisq.test(ATP_v2$sex, ATP_v2$kindness_online) #delete
chisq.test(ATP_v2$sex, ATP_v2$problem_harass)
chisq.test(ATP_v2$sex, ATP_v2$problem_privacy)
chisq.test(ATP_v2$sex, ATP_v2$problem_false_info)
chisq.test(ATP_v2$sex, ATP_v2$problem_time_waste)
chisq.test(ATP_v2$sex, ATP_v2$welcome_socialnetwork)
chisq.test(ATP_v2$sex, ATP_v2$welcome_online_dating)
chisq.test(ATP_v2$sex, ATP_v2$welcome_website_comments) #delete
chisq.test(ATP_v2$sex, ATP_v2$welcome_online_gaming) #delete
chisq.test(ATP_v2$sex, ATP_v2$welcome_discussion_forum)
chisq.test(ATP_v2$sex, ATP_v2$offensive_content)
chisq.test(ATP_v2$sex, ATP_v2$online_freedom)
chisq.test(ATP_v2$sex, ATP_v2$untrue_post)
chisq.test(ATP_v2$sex, ATP_v2$experience_doxxing) #delete
chisq.test(ATP_v2$sex, ATP_v2$experience_swatting) #delete
chisq.test(ATP_v2$sex, ATP_v2$experience_hacking)
chisq.test(ATP_v2$sex, ATP_v2$experience_trolling)
chisq.test(ATP_v2$sex, ATP_v2$experience_none) #delete
chisq.test(ATP_v2$sex, ATP_v2$non_consent_photo_post) #delete
chisq.test(ATP_v2$sex, ATP_v2$non_consent_photo_sent) #delete
chisq.test(ATP_v2$sex, ATP_v2$personal_offensive_names) 
chisq.test(ATP_v2$sex, ATP_v2$personal_physical_threat)
chisq.test(ATP_v2$sex, ATP_v2$personal_harass_over_time) #delete
chisq.test(ATP_v2$sex, ATP_v2$personal_stalk) #delete
chisq.test(ATP_v2$sex, ATP_v2$personal_embarrass) #delete
chisq.test(ATP_v2$sex, ATP_v2$personal_sexual_harass)
chisq.test(ATP_v2$sex, ATP_v2$harass_count)
chisq.test(ATP_v2$sex, ATP_v2$multiple_platform) #delete
chisq.test(ATP_v2$sex, ATP_v2$env_social_network) #delete
chisq.test(ATP_v2$sex, ATP_v2$env_online_dating)
chisq.test(ATP_v2$sex, ATP_v2$env_website_comments) #delete
chisq.test(ATP_v2$sex, ATP_v2$env_online_gaming) #delete
chisq.test(ATP_v2$sex, ATP_v2$env_email) #delete
chisq.test(ATP_v2$sex, ATP_v2$env_discussion_forum) #delete
chisq.test(ATP_v2$sex, ATP_v2$env_text) #delete
chisq.test(ATP_v2$sex, ATP_v2$env_other_app) #delete
chisq.test(ATP_v2$sex, ATP_v2$multi_people) #delete
chisq.test(ATP_v2$sex, ATP_v2$still_consider_harassment)
chisq.test(ATP_v2$sex, ATP_v2$danger_risk) #delete

### creating the modes for the variables
### creating data tables
increase_focus_harassment_interact <- as.data.frame(table(ATP_v2$increase_focus_harassment_interact))
harass_experience <- as.data.frame(table(ATP_v2$harass_experience))
harass_setting <- as.data.frame(table(ATP_v2$harass_setting))
women_obstacles <- as.data.frame(table(ATP_v2$women_obstacles))
problem_harass <- as.data.frame(table(ATP_v2$problem_harass))
problem_privacy <- as.data.frame(table(ATP_v2$problem_privacy))
problem_false_info <- as.data.frame(table(ATP_v2$problem_false_info))
problem_time_waste <- as.data.frame(table(ATP_v2$problem_time_waste))
welcome_socialnetwork <- as.data.frame(table(ATP_v2$welcome_socialnetwork))
welcome_online_dating <- as.data.frame(table(ATP_v2$welcome_online_dating))
welcome_discussion_forum <- as.data.frame(table(ATP_v2$welcome_discussion_forum))
offensive_content <- as.data.frame(table(ATP_v2$offensive_content))
online_freedom <- as.data.frame(table(ATP_v2$online_freedom))
untrue_post <- as.data.frame(table(ATP_v2$untrue_post))
experience_hacking <- as.data.frame(table(ATP_v2$experience_hacking))
experience_trolling <- as.data.frame(table(ATP_v2$experience_trolling))
personal_offensive_names <- as.data.frame(table(ATP_v2$personal_offensive_names))
personal_physical_threat <- as.data.frame(table(ATP_v2$personal_physical_threat))
personal_sexual_harass <- as.data.frame(table(ATP_v2$personal_sexual_harass))
upset_scale <- as.data.frame(table(ATP_v2$upset_scale))
env_online_dating <- as.data.frame(table(ATP_v2$env_online_dating))
still_consider_harassment <- as.data.frame(table(ATP_v2$still_consider_harassment))

### isolating mode
env_online_dating <- env_online_dating[2,]
experience_hacking <- experience_hacking[2,]
experience_trolling <- experience_trolling[2,]
harass_experience <- harass_experience[1,]
harass_setting <- harass_setting[1,]
increase_focus_harassment_interact <- increase_focus_harassment_interact[2,]
offensive_content <- offensive_content[1,]
online_freedom <- online_freedom[2,]
personal_offensive_names <- personal_offensive_names[2,]
personal_physical_threat <- personal_physical_threat[2,]
personal_sexual_harass <- personal_sexual_harass[2,]
problem_false_info <- problem_false_info[1,]
problem_harass <- problem_harass[1,]
problem_privacy <- problem_privacy[1,]
problem_time_waste <- problem_time_waste[1,]
still_consider_harassment <- still_consider_harassment[3,]
untrue_post <- untrue_post[1,]
upset_scale <- upset_scale[1,]
welcome_discussion_forum <- welcome_discussion_forum[1,]
welcome_online_dating <- welcome_online_dating[1,]
welcome_socialnetwork <- welcome_socialnetwork[1,]
women_obstacles <- women_obstacles[2,]

### combining into one dataframe
Variable_modes <- bind_rows(increase_focus_harassment_interact, harass_experience, 
                            harass_setting, women_obstacles, problem_harass, 
                            problem_privacy, problem_false_info, problem_time_waste, 
                            welcome_socialnetwork, welcome_online_dating, welcome_discussion_forum, 
                            offensive_content, online_freedom, untrue_post, experience_hacking, 
                            experience_trolling, personal_offensive_names, 
                            personal_physical_threat, personal_sexual_harass, 
                            upset_scale, env_online_dating, still_consider_harassment)

### Creating Variable names
Variable_Name <- c("increase_focus_harassment interact", "harass_experience", 
                   "harass_setting", "women_obstacles", "problem_harass", 
                   "problem_privacy", "problem_false_info", "problem_time_waste", 
                   "welcome_socialnetwork", "welcome_online_dating", 
                   "welcome_discussion_forum", "offensive_content", 
                   "online_freedom", "untrue_post", "experience_hacking", 
                   "experience_trolling", "personal_offensive_names", "personal_physical_threat", 
                   "personal_sexual_harass", "upset_scale", "env_online_dating", 
                   "still_consider_harassment")
Variable_Name <- as.data.frame(Variable_Name)

###re-combine dataframe
Variable_modes_final <- cbind(Variable_Name, Variable_modes)
names(Variable_modes_final)[2] <- "Answer_Choice"

### creating crosstables for variables
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$increase_focus_harassment_interact, 
                 title = "Has the increased focus on sexual harassment made it harder/easier 
                 for men to interact with women in the workplace?", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$harass_experience, 
                 title = "Have you ever received unwanted sexual advances or verbal 
                 or physical harassment of a sexual nature?", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$harass_setting, 
                 title = "Have you received unwanted sexual advances or verbal or 
                 physical harassment of a sexual nature...?", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$women_obstacles, 
                 title = "Which statement comes closer to your own views: 
                 A) The obstacles that once made it harder for women than men to 
                 get ahead are now largely gone; B) There are still significant obstacles 
                 that make it harder for women to get ahead than men", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$problem_harass, 
                 title = "How much of a problem, if at all, are the following: 
                 People being harassed or bullied", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$problem_false_info, 
                 title = "How much of a problem, if at all, are the following: 
                 People seeing false or inaccurate information", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$problem_time_waste, 
                 title = "How much of a problem, if at all, are the following: 
                 People spending too much time online", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$welcome_socialnetwork, 
                 title = "Do you think each of the online environments below are 
                 more welcoming toward men, women, or both?: Social networking 
                 sites/apps", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$welcome_online_dating, 
                 title = "Do you think each of the online environments below are 
                 more welcoming toward men, women, or both?: Online dating websites/apps", 
                 show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$welcome_discussion_forum, 
                 title = "Do you think each of the online environments below are 
                 more welcoming toward men, women, or both?: Online discussion sites 
                 such as Reddit", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$offensive_content, 
                 title = "Which comes closer to your view: A) Offensive content 
                 online is too often excused as not a big deal; B) Many people 
                 take offensive content they see online too seriously", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$online_freedom, 
                 title = "Which do you think is more important: A) People being 
                 able to speak their minds freely online; B) People being able to 
                 feel welcome and safe online", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$untrue_post, title = 
                   "Has someone ever posted something about you online that was 
                 not true?", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$experience_hacking, 
                 title = "Have any of these ever happened to you, personally: 
                 Hacking", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$experience_trolling, 
                 title = "Have any of these ever happened to you, personally: 
                 Trolling", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$personal_offensive_names, 
                 title = "Which of the following have happened to you, personally, 
                 ONLINE?: Been called offensive names", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$personal_physical_threat, 
                 title = "Which of the following have happened to you, personally, 
                 ONLINE?: Been physically threatened", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$personal_sexual_harass, 
                 title = "Which of the following have happened to you, personally, 
                 ONLINE?: Been sexually harassed", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$upset_scale, 
                 title = "Still thinking about your most recent harassment experience, 
                 how upsetting was this, if at all?", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$env_online_dating, 
                 title = "In which of the following online environments did your 
                 most recent occur: Online Dating", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = ATP_v2$sex, var.col = ATP_v2$still_consider_harassment, 
                 title = "Still thinking about your most recent experience, do 
                 you consider what happened to you to be online harassment or not?", 
                 show.row.prc = TRUE)

### visualizations for data 
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$increase_focus_harassment_interact, 
                  margin = "row", bar.pos = "dodge", coord.flip = FALSE, 
                  title="Has the increased focus on sexual harassment made it harder/easier 
                  for men to interact with women in the workplace?", show.n = FALSE, 
                  show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$harass_experience, margin = "row", bar.pos = "dodge", 
                  coord.flip = FALSE, title="Have you ever received unwanted sexual 
                  advances or verbal or physical harassment of a sexual nature?", 
                  show.n = FALSE, show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$harass_setting, margin = "row", bar.pos = "dodge", 
                  coord.flip = FALSE, title="Locations where respondent received 
                  sexual harassment", show.n = FALSE, show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$women_obstacles, margin = "row", bar.pos = "dodge", 
                  coord.flip = FALSE, title="The obstacles that once made it harder 
                  for women than men to get ahead are now largely gone VS There are 
                  still significant obstacles that make it harder for women to get 
                  ahead than men", show.n = FALSE, show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$problem_harass, margin = "row", bar.pos = "dodge", 
                  coord.flip = FALSE, title="Problem - People being harassed or 
                  bullied", show.n = FALSE, show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$problem_false_info, margin = "row", bar.pos = "dodge", 
                  coord.flip = FALSE, title="Problem - False or inaccurate information 
                  being shown", show.n = FALSE, show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$time_waste, margin = "row", bar.pos = "dodge", 
                  coord.flip = FALSE, title="Problem - People spending too much 
                  time online", show.n = FALSE, show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$problem_time_waste, margin = "row", bar.pos = "dodge", 
                  coord.flip = FALSE, title="Problem - People spending too much time online", 
                  show.n = FALSE, show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$welcome_socialnetwork, margin = "row", bar.pos = "dodge", 
                  coord.flip = FALSE, title="More welcoming toward men, women, or 
                  both - Social networking sites/apps", show.n = FALSE, show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$welcome_online_dating, margin = "row", bar.pos = "dodge", 
                  coord.flip = FALSE, title="More Welcoming toward men, women, or 
                  both - Online dating websites/apps", show.n = FALSE, show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$welcome_discussion_forum, margin = "row", bar.pos = "dodge", 
                  coord.flip = FALSE, title="More Welcoming toward men, women, or 
                  both - Online discussion sites such as Reddit", show.n = FALSE, show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$offensive_content, margin = "row", bar.pos = "dodge", 
                  coord.flip = FALSE, title="Offensive content online is too often excused 
                  as not a big deal VS Many people take offensive content they see 
                  online too seriously", show.n = FALSE, show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$online_freedom, margin = "row", bar.pos = "dodge", 
                  coord.flip = FALSE, title="People being able to speak their minds 
                  freely online VS People being able to feel welcome and safe online", 
                  show.n = FALSE, show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$untrue_post, margin = "row", bar.pos = "dodge", 
                  coord.flip = FALSE, title="Has someone ever posted something about 
                  you online that was not true?", show.n = FALSE, show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$experience_hacking, margin = "row", bar.pos = "dodge", 
                  coord.flip = FALSE, title="Have any of these ever happened to you 
                  personally: Hacking", show.n = FALSE, show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$experience_trolling, margin = "row", bar.pos = "dodge", 
                  coord.flip = FALSE, title="Have any of these ever happened to you 
                  personally: Trolling", show.n = FALSE, show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$personal_offensive_names, margin = "row", 
                  bar.pos = "dodge", coord.flip = FALSE, title="Which of the 
                  following have happened to you personally, ONLINE?: Been called 
                  offensive names", show.n = FALSE, show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$personal_physical_threat, margin = "row", 
                  bar.pos = "dodge", coord.flip = FALSE, title="Which of the following 
                  have happened to you personally, ONLINE?: Been physically threatened", 
                  show.n = FALSE, show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$personal_sexual_harass, margin = "row", 
                  bar.pos = "dodge", coord.flip = FALSE, title="Which of the following 
                  have happened to you personally, ONLINE?: Been sexually harassed", 
                  show.n = FALSE, show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$upset_scale, margin = "row", bar.pos = "dodge", 
                  coord.flip = FALSE, title="Still thinking about your most recent 
                  harassment experience, how upsetting was this, if at all?", 
                  show.n = FALSE, show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$env_online_dating, margin = "row", bar.pos = "dodge", 
                  coord.flip = FALSE, title="In which of the following environments 
                  did your most recent occur?: Online Dating", show.n = FALSE, 
                  show.prc = FALSE)
sjPlot::plot_xtab(ATP_v2$sex, ATP_v2$still_consider_harassment, margin = "row", 
                  bar.pos = "dodge", coord.flip = FALSE, title="Still thinking about 
                  your most recent experience, do you consider what happened to 
                  you to be online harassment or not?", show.n = FALSE, show.prc = FALSE)







