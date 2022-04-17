# Jesse Wise 2022 Undergraduate Dissertation
# Nudging for Cooperation - How Does Valence Framing Affect Contributions in a Public Goods Game?
# Please cite where appropriate

#Loading libraries
library(HLMdiag)
library(broom.mixed)
library(apaTables)
library(lmtest)
library(ggridges)
library(ICC)
library(DHARMa)
library(ggpubr)
library(remotes)
library(splitstackshape)
library(glmmTMB)
library(rstatix)
library(psych)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(purrr)
library(lattice)
library(lme4)
library(stringr)
library(cowplot)
library(sjPlot)
library(broom.mixed)
library(gridExtra)
library(grid)
library(performance)
library(effects)
library(boot)
library(lmerTest)
library(interactions)
library(pequod)
library(reghelper)
library(robustlmm)

rm(list = ls())
#used to identify prompts
str_id <- "<p id"
list_of_id<-c('pollution_health',
              'ocean_acid',
              'nat_sec',
              'econ_warfare',
              'business',
              'comp_adv',
              'heat_death',
              'deforrest',
              'green_job_health',
              'intersectional',
              'disease',
              'plastic_pol',
              'sea_levels',
              'cycle_health',
              'com_garden',
              'green_econ_boost')
z<-0
other<-c(0.60, 0.58, 0.49, 0.55, 0.39, 0.43, 0.37, 0.41, 0.36, 0.31, 0.34, 0.23, 0.29, 0.1, 0.20)

# Reading in data files
setwd("~/UnderGrad/UGYr4/Dissertation/Code/Prolific data")

files <-list.files()#list.files("~/UnderGrad/UGYr4/Dissertation/Code/Prolific data")

files<-files[endsWith(files, ".csv")]

df_dirty<-data.frame(prolific_id = numeric(),
                     framing=numeric(),
                     age = numeric(),
                     gender=numeric(),
                     income=numeric(),
                     response=character(),
                     stimulus=character(),
                     round=numeric(),
                     contribution=numeric(),
                     other_balance=numeric(),
                     current_balance=numeric(),
                     bonus=numeric())
#Loop over participants
for (i in 1:length(files)) {
  
  tmp <- read.csv(files[i])
  tmp2<-data.frame(prolific_id = rep(NA, 16),
                   framing=rep(NA, 16),
                   age = rep(NA, 16),
                   gender=rep(NA, 16),
                   income=rep(NA, 16),
                   response=rep(NA, 16),
                   stimulus=rep(NA, 16),
                   round=rep(NA, 16),
                   contribution=rep(NA, 16),
                   other_balance=rep(NA, 16),
                   current_balance=rep(NA, 16),
                   bonus=rep(NA,16))
    
  #print the file number of those who did not complete the experiment
  if(nrow(tmp)<73){
    print(files[i]) } else {
  #Loop over columns
  for(j in 1:ncol(df_dirty)){
    
    if (names(df_dirty)[j] %in% names(tmp)){
    #put into the column of tmp2 <- what is stored in tmp colum of the same name if it does not contain an NA
        tmp2[[names(df_dirty)[j]]]<-tmp[[names(df_dirty)[j]]][!is.na(tmp$contribution)]
    }
  }
  #save the entire MBI response into each row
      for (k in nrow(tmp2)){
        tmp2$response<-tmp$response[length(tmp$response)-1]
  }
  #saving the stimulus per trial
      df_stimulus<-(tmp%>%
         filter(str_detect(stimulus, "<p id")))
      for (j in 1:16){
        for (i in 1:16) {
         if(str_detect(df_stimulus$stimulus[j], list_of_id[i])){
              tmp2$stimulus[j]<-list_of_id[i]
            }
        }
        }
    }
  #then bind these rows stored in tmp2 to the end of df_dirty
  df_dirty <- rbind(df_dirty, tmp2)
}

#now we split all the info stored in this cell into individual columns
colnames(df_dirty)[which(names(df_dirty) == "response")] <- "mbi"
df_dirty<-cSplit(df_dirty, "mbi",",",)  

#check the data
head(df_dirty)
tail(df_dirty)

#Renaming the variable "round" to trial to avoid problems. Turning ID, Framing Gender and Income into factors. Age as numeric
names(df_dirty)[names(df_dirty) == "round"] <- "trial"
names(df_dirty)[names(df_dirty) == "prolific_id"] <- "id"
df_dirty$age<-as.numeric(df_dirty$age)

#Cleaning MBI scores
mbi_names<-  c("emotional_exhaustion", "tiredPM", "guilt", "burntout", "hopeless", "change_possible", "care_for_life", "wits_end")
for (i in 1:8){
colnames(df_dirty)[11+i] <- mbi_names[i]
}
df_dirty$emotional_exhaustion<-parse_number(df_dirty$emotional_exhaustion)
df_dirty$tiredPM<-parse_number(df_dirty$tiredPM)
df_dirty$guilt<-parse_number(df_dirty$guilt)
df_dirty$burntout<-parse_number(df_dirty$burntout)
df_dirty$hopeless<-parse_number(df_dirty$hopeless)
df_dirty$change_possible<-parse_number(df_dirty$change_possible)
df_dirty$care_for_life<-parse_number(df_dirty$care_for_life)
df_dirty$wits_end<-parse_number(df_dirty$wits_end)
df_dirty$av_mbi <- rowMeans(df_dirty[,12:19])

#contribution as a percentage of balance
df_dirty$contribution_pct<-(df_dirty$contribution/df_dirty$current_balance)*100

#Making a clean data set
df_clean<-df_dirty
#making income/framing/gender variables into factors
df_clean <-
  mutate_at(df_clean, vars(id, gender,stimulus), as.factor)
  
df_clean$income<-factor(df_clean$income, 
                        ordered=TRUE,
                        level=c ("low", "middle", "high", "notDisclosed"),
                        labels=c("Low (<£10k)", "Middle (£10k<x<£30k)", "High (>£30K)", "Not Disclosed"))
df_clean$framing <- factor(df_clean$framing,
                    levels = c(0,1,2),
                    labels = c("Control", "Positive", "Negative"))
df.tw <- df_clean
#df.sw is my subject wise data frame i.e one row per participant. It contains constants for a subject
#create df.sw using some tidyr functions
df.sw <-
  df.tw %>% group_by(id) %>% summarise(
    age = age[1],
    framing=framing[1],
    gender = gender[1],
    income= income[1],
    total_contribution = sum(contribution),
    average_contribution = mean(contribution),
    bonus=bonus[1],
    av_mbi=av_mbi[1],
    emotional_exhaustion=emotional_exhaustion[1], 
    tiredPM= tiredPM[1], 
    guilt=guilt[1], 
    burntout=burntout[1], 
    hopeless=hopeless[1], 
    change_possible=change_possible[1], 
    care_for_life=care_for_life[1], 
    wits_end= wits_end[1],
    t_1=contribution[1],
    t_2=contribution[2],
    t_3=contribution[3],
    t_4=contribution[4],
    t_5=contribution[5],
    t_6=contribution[6],
    t_7=contribution[7],
    t_8=contribution[8],
    t_9=contribution[9],
    t_10=contribution[10],
    t_11=contribution[11],
    t_12=contribution[12],
    t_13=contribution[13],
    t_14=contribution[14],
    t_15=contribution[15]
  )
#same data frame but contirbution stored as a percentage
df.sw.pct <-
  df.tw %>% group_by(id) %>% summarise(
    age = age[1],
    framing=framing[1],
    gender = gender[1],
    income= income[1],
    total_contribution = sum(contribution),
    average_contribution = mean(contribution),
    bonus=bonus[1],
    av_mbi=av_mbi[1],
    emotional_exhaustion=emotional_exhaustion[1], 
    tiredPM= tiredPM[1], 
    guilt=guilt[1], 
    burntout=burntout[1], 
    hopeless=hopeless[1], 
    change_possible=change_possible[1], 
    care_for_life=care_for_life[1], 
    wits_end= wits_end[1],
    t_1=contribution_pct[1],
    t_2=contribution_pct[2],
    t_3=contribution_pct[3],
    t_4=contribution_pct[4],
    t_5=contribution_pct[5],
    t_6=contribution_pct[6],
    t_7=contribution_pct[7],
    t_8=contribution_pct[8],
    t_9=contribution_pct[9],
    t_10=contribution_pct[10],
    t_11=contribution_pct[11],
    t_12=contribution_pct[12],
    t_13=contribution_pct[13],
    t_14=contribution_pct[14],
    t_15=contribution_pct[15]
  )

head(df.sw, 3)
head(df.sw.pct, 3)

#################### Calculating bonus payment
prolific_data<-c("id", "bonus")
df_prolific<-df.sw[prolific_data]
df_prolific$bonus<-round(as.numeric(df_prolific$bonus), 2)
summary(df_prolific)
sprintf("Total bonus payments is £%s. Paying £1 per participant with %s participants will cost £%s. Paying participants is £%s in total, or £%s including prolific's service fee.", sum(df_prolific$bonus), nrow(df_prolific), nrow(df_prolific), (nrow(df_prolific)+ sum(df_prolific$bonus)), ((nrow(df_prolific)+ sum(df_prolific$bonus))/0.6))
write.csv(df_prolific,"~/UnderGrad/UGYr4/Dissertation/Code\\bonus.csv", row.names = FALSE)
#have to save it as a CSV again and then open in text editor

###########################################################Exploring the data #######################################
summary(df.sw)
tab_df(df.sw, title="A table to show the the data set")  #summary of all subjects
summary_df.sw.pct<-summary(df.sw.pct)

summaryF<-df.tw %>%
  group_by(framing) %>%
  get_summary_stats(contribution, type = "mean_sd")
data.frame(summaryF)
tab_df(summary, title= "A table to show the mean contribution in every trial per condition")

write.csv(summary_df.sw.pct,"~/UnderGrad/UGYr4/Dissertation/Code\\summary_df.sw.pct", row.names = FALSE)

########################## Demographics
while (!is.null(dev.list()))  dev.off()
barplot(prop.table(table(df.sw$income)), main="A histogram to show the distribution of income levels", xlab="Self-described income level", ylab="Frequency")
barplot(prop.table(table(df.sw$gender)), main="A histogram to show the distribution of gender", xlab="Self-described gender", ylab="Frequency")
hist(df.sw$age, main="A histogram to show the disribution of age (in years)", xlab="Age (years)")
md_demographics<-rlmer(contribution~framing+ av_mbi+trial+ age+gender+income+ (1|id), data=df_clean)
summary(md_demographics)
sjPlot::tab_model(md_demographics)


###### Identify outlines
df.tw %>%
  group_by(trial) %>%
  identify_outliers(contribution)
# A lot of outliers! What happens if we get rid of them?
Q1 <- quantile(df.tw$contribution, .25)
Q3 <- quantile(df.tw$contribution, .75)
IQR <- IQR(df.tw$contribution)
df.tw_clean <- subset(df.tw, df.tw$contribution> (Q1 - 1.5*IQR) & df.tw$contribution< (Q3 + 1.5*IQR))

################################################################ Stimulus
###### Check contribution by stimulus in each framing condition
ggboxplot(df_clean, x = "stimulus", y = "contribution", add = "point", main="Contirbution per stimulus across subjects", color="framing")
ggboxplot(df_clean, x = "stimulus", y = "contribution_pct", add = "point", main="Contirbution (as a %) per stimulus across subjects", color="framing")

###### Are stimuli evenly distributed across trials overall?
ggboxplot(df_clean, x = "stimulus", y = "trial", add = "point", main="Distribution of stimulus over trials across subjects")
ggboxplot(df_clean, x = "stimulus", y = "trial", add = "point", main="Distribution of stimulus over trials across subjects within in each treatment", color="framing")

###### Does stimulus have an effect on contribution?
md_stim<-lm(contribution_pct~stimulus, data=df.tw)
summary(md_stim)
sjPlot::tab_model(md_stim)
anova(md_stim)

md_stim_fra<-lm(contribution_pct~stimulus*framing, data=df.tw)
summary(md_stim_fra)
sjPlot::tab_model(md_stim_fra)
anova(md_stim_fra)
# No interaction with frames, and subject didn't really respond to the prompts themselves.
 compare_performance(md_stim, md_stim_fra)
################################################################ MBI
hist(df.sw$av_mbi, main="Distribution of average scores on measures of Emotional Exhaustion", xlab="Average score")
df_sw_mbi<- df.sw[][10:17]
tab_stackfrq(df_sw_mbi, show.n=T, title="A table to show the distribution of responses to Emotional Exhasution measures", alternate.rows = T, var.labels=c("When I think about the biggest challenges of the 21st century, I feel emotionally exhausted.", "I feel worn out at the end of a day.", "I feel guilty when I'm not doing something to improve human rights or social justice.", "I feel burnt out because of the state of the world.", "When I think about the injustices in the world, I feel hopeless.", "I feel hopeful that social change is possible.", "I value and respect all life. I care about people I am not related to, have never met, nor will ever meet.", "When I learn about current affairs I feel as if I'm at my wits' end."), value.labels=c("I don't ever think about this or feel this way", "A few times a year", "Around once a month", "Several times a month", "About once a week", "Nearly every day"))

###### Does average level of burnout have an effect?
md_MBI<-lmer(contribution~trial + av_mbi+age+gender+income+ (1|id), data=df_clean)
summary(md_MBI)
sjPlot::tab_model(md_MBI)

###### Do any of the measures in particular have an effect?
md_MBI_detail<-lmer(contribution_pct ~emotional_exhaustion + tiredPM + guilt+ burntout+hopeless+change_possible+care_for_life+wits_end + age + gender + income + (1|id), data=df_clean)
summary(md_MBI_detail)
sjPlot::tab_model(md_MBI_detail)

###### Does average mbi scores improve model fit compared to my null?
anova(md_demographics,md_MBI)
anova(md_demographics, md_MBI_detail)
# mbi is an improvement in fit. Detailed mbi is a very very significant improvement

###### Distribution of each MBI question

par(mfrow=c(2,4))
hist(df.sw$emotional_exhaustion, xlab="When I think about the biggest challenges of \nthe 21st century, I feel emotionally exhausted.", main="" )
hist(df.sw$tiredPM, xlab="I feel worn out at the end of a day", main="")
hist(df.sw$guilt, xlab="I feel guilty when I'm not doing something to \nimprove human rights or social justice.", main="")
hist(df.sw$burntout, xlab= "I feel burnt out because of the state of the world", main="")
hist(df.sw$hopeless, xlab="When I think about the injustices \nin the world, I feel hopeless.", main="")
hist(df.sw$change_possible, xlab= "I feel hopeful that social change is possible", main="")
hist(df.sw$care_for_life, xlab = "I value and respect all life. I care about people I \nam not related to, have never met, nor will ever meet.", main="")
hist(df.sw$wits_end, xlab="When I learn about current \naffairs I feel as if I'm at my wits' end", main="")
title("Frequency of Responses per Measure of Emotional Exhaustion (Likert Scale, 0 indicates NA)", line = -1, outer = TRUE)


################################################## Visualizing the data #######################################
#################### By trial
ggboxplot(df.tw, x = "trial", y = "contribution", add = "point", main="Contirbution per trial across subjects", colour="framing")
ggboxplot(
  df.tw, x = "trial", y = "contribution",
  color = "framing",
  main= "Absolute contribution per trial in each condition")
xyplot(contribution~trial|framing, data=df.tw, main="Individual's absolute contirbution per trial in each condition", group=id, type="b")

##### Now contribution as a pct
ggboxplot(
   df.tw, x = "trial", y = "contribution_pct",
   color = "framing",
   main= "Contribution(%) per trial in each condition")
xyplot(contribution_pct~trial|framing, data=df.tw, group=id, type="b", main="Contribution(%) per trial in each condition")

########## Looking at the average contribution in each trial  
average_contribution_by_frame_mean<-aggregate(contribution_pct~framing + trial, df.tw, mean)
plot_average_contribution_by_frame_mean<-xyplot(average_contribution_by_frame_mean$contribution_pct~trial|framing, data=average_contribution_by_frame_mean, group=framing, tybe="b", xlab="trial", ylab="average contibution", main= "Mean contribution (%) across subjects", panel=function(x,y){
  panel.xyplot(x,y,grid=TRUE,
               tyoe=c("p", "smooth"),
               col.line="black")
  panel.lmline(x, y)
})

#### control for mbi
average_contribution_by_frame_mean_mbi<-aggregate(contribution_pct~framing + trial + av_mbi, df.tw, mean)
plot_average_contribution_by_frame_mean_mbi<-xyplot(average_contribution_by_frame_mean_mbi$contribution_pct~trial|framing, data=average_contribution_by_frame_mean_mbi, group=framing, tybe="b", xlab="trial", ylab="average contibution", main= "Mean contribution (%) across subjects, controlling for MBI", panel=function(x,y){
  panel.xyplot(x,y,grid=TRUE,
               tyoe=c("p", "smooth"),
               col.line="black")
  panel.lmline(x, y)
})


#### What about the median?
average_contribution_by_frame_median<-aggregate(contribution_pct~framing + trial, df.tw, median)
plot_average_contribution_by_frame_median<-xyplot(average_contribution_by_frame_median$contribution_pct~trial|framing, data=average_contribution_by_frame_median, group=framing, tybe="b", xlab="trial", ylab="average contibution", main= "Median contribution (%) across subjects", panel=function(x,y){
  panel.xyplot(x,y,grid=TRUE,
               tyoe=c("p", "smooth"),
               col.line="black")
  panel.lmline(x, y)
})


#### control for mbi
average_contribution_by_frame_median_mbi<-aggregate(contribution_pct~av_mbi + framing + trial, df.tw, median)
plot_average_contribution_by_frame_median_mbi<-xyplot(average_contribution_by_frame_median_mbi$contribution~trial|framing, data=average_contribution_by_frame_median_mbi, group=framing, tybe="b", xlab="trial", ylab="average contibution", main= "Median contribution (%) across subjects, controlling for MBI", panel=function(x,y){
  panel.xyplot(x,y,grid=TRUE,
               tyoe=c("p", "smooth"),
               col.line="black")
  panel.lmline(x, y)
})

#### mean without outliers
average_contribution_by_frame_mean_outliers<-aggregate(contribution_pct~ framing + trial, df.tw_clean, mean)
plot_average_contribution_by_frame_mean_outliers<-xyplot(average_contribution_by_frame_mean_outliers$contribution_pct~trial|framing, data=average_contribution_by_frame_mean_outliers, group=framing, tybe="b", xlab="trial", ylab="average contibution", main= "Mean contribution (%) across subjects (outliers removed)", panel=function(x,y){
  panel.xyplot(x,y,grid=TRUE,
               tyoe=c("p", "smooth"),
               col.line="black")
  panel.lmline(x, y)
})

# median without outliers
average_contribution_by_frame_median_outliers<-aggregate(contribution_pct~ framing + trial, df.tw_clean , median)
plot_average_contribution_by_frame_median_outliers<-xyplot(average_contribution_by_frame_median_outliers$contribution_pct~trial|framing, data=average_contribution_by_frame_median_outliers, group=framing, tybe="b", xlab="trial", ylab="average contibution", main= "Median contribution (%) across subjects (outliers removed)", panel=function(x,y){
  panel.xyplot(x,y,grid=TRUE,
               tyoe=c("p", "smooth"),
               col.line="black")
  panel.lmline(x, y)
})

######### Ploting
grid.arrange(plot_average_contribution_by_frame_mean, plot_average_contribution_by_frame_mean_outliers, plot_average_contribution_by_frame_mean_mbi, plot_average_contribution_by_frame_median, plot_average_contribution_by_frame_median_outliers, plot_average_contribution_by_frame_median_mbi, ncol=3)

###### Plot individual's responses and the median
average_contribution_by_frame<-aggregate(contribution_pct~ framing + trial, df.tw ,mean)
average_contribution_by_frame<- average_contribution_by_frame %>%
  gather(key = "trial", value = "contribution", framing) 
gg.base <- ggplot(df.tw, aes(x = trial, y = contribution_pct))
gg.Gline <- gg.base + geom_line(aes(color = framing, group = id))
gg.Gline + geom_point()
gg.Gline + stat_summary(aes(group = framing, color = paste("mean", framing)),
                        geom = "line", fun = mean, size = 2) +
  scale_colour_manual(name = "Group",
                      values = c("Control" = "indianred1", "mean Control" = "red",
                                 "Positive" = "darkgreen", "mean Positive" = "green",
                                 "Negative" = "deepskyblue", "mean Negative" = "blue"))
 
###### Plot just the mean and median response by themselves
 line_mean<- gg.base + stat_summary(data=df.tw, aes(x=trial, y=contribution_pct, group = framing, color = framing),
                        geom = "line",  fun = mean, size = 2) + ggtitle ("Mean contributions per trial in each condition")
 line_median<-gg.base + stat_summary(data=df.tw, aes(x=trial, y=contribution_pct, group = framing, color = framing),
                        geom = "line", fun = median, size = 2) + ggtitle ("Median contributions per trial in each condition")
grid.arrange(line_mean, line_median, nrow=1)

############################################## Statistical Analysis ##################################################
#### Prep the data
df<- df.tw
df<-left_join(df, df_clean %>% select(id, trial, current_balance)) 
df<- df %>% mutate(ID = factor(id),
                   proportion = contribution/current_balance,
                   prop_nonzero = proportion)
df$trial <- as.numeric(gsub("t_","", df$trial))+1
df$prop_nonzero[df$prop_nonzero==0]<-0.00001
df$prop_nonzero[df$prop_nonzero==1]<-0.9999
df<-df %>% mutate(time1 = poly(trial,1, raw=T)[,1], time2 = poly(trial,2, raw=T)[,2], time3 = poly(trial,3, raw=T)[,3])
df$logit_prop_nonzero<-logit(df$prop_nonzero)
df$lagged_contribution<-NA
df$lagged_contribution[2:nrow(df)]<-df$prop_nonzero[1:(nrow(df)-1)]
df$lagged_contribution[df$trial==1]<-NA

# Do the same for my subject wise data frame
dfsw <-
  df %>% group_by(id) %>% summarise(
    age = age[1],
    framing=framing[1],
    gender = gender[1],
    income= income[1],
    average_contribution = mean(prop_nonzero),
    bonus=bonus[1],
    av_mbi=av_mbi[1],
    emotional_exhaustion=emotional_exhaustion[1], 
    tiredPM= tiredPM[1], 
    guilt=guilt[1], 
    burntout=burntout[1], 
    hopeless=hopeless[1], 
    change_possible=change_possible[1], 
    care_for_life=care_for_life[1], 
    wits_end= wits_end[1],
    t_1=prop_nonzero[1],
    t_2=prop_nonzero[2],
    t_3=prop_nonzero[3],
    t_4=prop_nonzero[4],
    t_5=prop_nonzero[5],
    t_6=prop_nonzero[6],
    t_7=prop_nonzero[7],
    t_8=prop_nonzero[8],
    t_9=prop_nonzero[9],
    t_10=prop_nonzero[10],
    t_11=prop_nonzero[11],
    t_12=prop_nonzero[12],
    t_13=prop_nonzero[13],
    t_14=prop_nonzero[14],
    t_15=prop_nonzero[15]
  )

############################ Checking distributions and which model to use

###### Check normality assumptions
dev.off()
qqnorm(df$prop_nonzero, main='Contributions in each trial')
qqline(df$prop_nonzero)
df %>%
  group_by(trial) %>%
  shapiro_test(prop_nonzero)
idplots <- 
  ggplot(df, aes(x = trial, y = contribution_pct, 
                 col = framing)) +
  geom_point()+
  facet_wrap(~id) + 
  guides(col = "none") +
  labs(x = "Trial", 
       y = "Contribution (%)")
idplots

############## Does a beta, binomial or gaussian distribution fit my data better? 
md1.3<-lmer(prop_nonzero~framing + time1 + time2 + (1|id), data=df)
alt_beta_md1.3<-glmmTMB(prop_nonzero ~ time1 + time2 + framing + (1 |id), data = df, family ='beta_family')
alt_binomial_md1.3<-glmer(prop_nonzero ~ time1 + time2 + framing + (1 |id), data = df, family ='binomial')


##### Compare summaries
sjPlot::tab_model(md1.3)
sjPlot::tab_model(alt_beta_md1.3)
sjPlot::tab_model(alt_binomial_md1.3)

### testing the models
compare_performance(md1.3, alt_beta_md1.3, alt_binomial_md1.3)
#looks like beta model is best distribution, best R2 and AIC and BIC with 2nd best RMSE, good ICC too. But results are  uninterpretable. So I will continue with gaussian distribution


########################################################################## Running my models
#This is what what we would expect to see according to the literature. We know there's a standard pattern of response and this is what it would look like
md_null<-lmer(prop_nonzero ~ time1 + (1|id), data=df)
summary(md_null)
sjPlot::tab_model(md_null)
# This is what we see in the literature

############################### Model numbers refer to pre-registration
######## MODEL 0 - How does framing affect AVERAGE contribution?
md1.0<-lm(average_contribution~framing , data=dfsw)
summary(md1.0)
sjPlot::tab_model(md1.0)

######## MODEL 1 - How does framing and trial affect INDIVIDUAL contribution?
md1.1<-lmer(prop_nonzero~framing + time1 +(1|id) , data=df)
summary(md1.1)
augment(md1.1) %>% ggplot(., aes(x=time1,y=.fitted,color=framing)) + stat_summary(geom="line") + stat_summary(geom="pointrange", aes(y=prop_nonzero)) 
sjPlot::tab_model(md1.1)
#not sure how to interpret these coefficients, how can time lead to a more than 100% contributioN?
plot(allEffects(md1.1))
plot_model(md1.1, type="pred")

######## MODEL 2 - Is there an interaction between time and framing? I.e. does the effect of framing change with time?
md1.2<-lmer(prop_nonzero~framing * time1 + (1|id), data=df)
summary(md1.2)
tab_model(md1.2)
augment(md1.2) %>% ggplot(., aes(x=time1,y=.fitted,color=framing)) + stat_summary(geom="line") + stat_summary(geom="pointrange", aes(y=prop_nonzero)) 
plot(allEffects(md1.2))
plot_model(md1.2, type="pred")

######## MODEL 3 - Introduce a lagged time variable and a polynomial of time into model 1.
md1.3<-lmer(prop_nonzero~framing * time1 + time2 + lagged_contribution + (1|id), data=df)
summary(md1.3)
sjPlot::tab_model(md1.3)
augment(md1.3) %>% ggplot(., aes(x=time1,y=.fitted,color=framing)) + stat_summary(geom="line") + stat_summary(geom="pointrange", aes(y=prop_nonzero)) 
plot(allEffects(md1.3))

######## MODEL 4 - Introduce an interaction between framing and emotional exhaustion to model 1
md1.4<-lmer(prop_nonzero~framing*av_mbi + time1 + (1|id), data=df)
summary(md1.4)
augment(md1.4) %>% ggplot(., aes(x=time1,y=.fitted,color=framing)) + stat_summary(geom="line") + stat_summary(geom="pointrange", aes(y=prop_nonzero)) 
sjPlot::tab_model(md1.4)
plot(allEffects(md1.4))

######## MODEL 5 - Controlling for emotional exhaustion in model 3 
md1.5<-lmer(prop_nonzero~framing *time1 + time2 + lagged_contribution + av_mbi + (1|id), data=df)
beta_md1.5<-glmmTMB(prop_nonzero~framing *time1 + time2 + lagged_contribution + av_mbi + (1|id), data = df, family ='beta_family')
summary(beta_md1.5)
summary(md1.5)


######## ROBUST SE MODEL 5 - Controlling for emotional exhaustion in model 3 
library(robustlmm)
robust_md1.5<-rlmer(prop_nonzero~framing *time1 + time2 + lagged_contribution + av_mbi + (1|id), data=df)
tab_model(robust_md1.5, show.fstat=T, show.re.var=T, show.stat = T, digits=4, vcov.type = "HC3", show.aic=T, title="A table to show results of a LMEM regression of model (1.5) with random intercepts, Gaussian distribution and robust standard errors")
tab_model(md1.5, show.fstat=T, show.re.var=T, show.stat = T, show.aic=T, title="A table to show results of a LMEM regression of model (1.5) with random intercepts and Gaussian distribution")

# Making a function for the other participant SO I can include this in a plot
trial<-2:16
other_function<-data.frame(trial, other)
md_other<-lm(other~trial ,data=other_function)
library(broom.mixed)
newdf <- subset(df,time1!=1)

# Plotting
augment(robust_md1.5) %>% ggplot(., aes(x=time1, y=.fitted, color=framing)) + stat_summary(geom="line") + stat_summary(geom="pointrange", aes(y=prop_nonzero)) + ggtitle("Model (1.5) with random intercepts and robust standard errors fitted to observed data")+ xlab("Trial") + ylab("Contribution(%)") +  geom_line(data = other_function, aes(x = trial, y = other, colour= "Fixed participant"))

ggplot(data = newdf, aes(x = time1, y = prop_nonzero)) +
  labs(y = "Contribution (%)", x = "Trial") +
  geom_smooth(method=lm, aes(y=predict(robust_md1.5), group=framing, colour=framing))+
  stat_summary(geom="pointrange", aes(y=predict(robust_md1.5), group=framing, colour=framing)) +
  geom_line(data = other_function, aes(x = trial, y = other, colour= "Fixed participant")) +
  ggtitle("Model (1.5) with random intercepts and robust standard errors fitted to observed data")


plot(x=predict(robust_md1.5), y=newdata$prop_nonzero,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')

# Plotting interaction
p_md1.5 <- probe_interaction(md1.5,
                                 pred=time1,
                                modx=framing,  
                                 mod2=av_mbi,
                                 cond.int = T,
                                 interval = T, 
                                 jnplot = T) 
p_md1.5
simple_slopes(md1.5)

######## MODEL 6 - 3 way interaction
md1.6<-lmer(prop_nonzero~framing *time1 *av_mbi + time2 +lagged_contribution +(1|id), data=df)
augment(md1.6) %>% ggplot(., aes(x=time1, y=.fitted, color=framing)) + stat_summary(geom="line") + stat_summary(geom="pointrange", aes(y=prop_nonzero)) +  ggtitle("Model (1.6) with random intercepts fitted to observed data")+ xlab("Trial") + ylab("Contribution(%)") +  geom_line(data = other_function, aes(x = trial, y = other, colour= "Fixed participant"))
anova(md1.5, md1.6_3way) # not an improvement


######## ROBUST MODEL 6 - 3 way interaction
robust_md1.6<-rlmer(prop_nonzero~framing *time1 *av_mbi + time2 +lagged_contribution +(1|id), data=df)
tab_model(robust_md1.6, show.fstat=T, show.re.var=T, show.stat = T, digits=4, vcov.type = "HC3", show.aic=T, title="A table to show results of a LMEM regression of model (1.6) with random intercepts, Gaussian distribution and robust standard errors")
ggplot(data = newdf, aes(x = time1, y = prop_nonzero)) +
  labs(y = "Contribution (%)", x = "Trial") +
  geom_smooth(method=lm, aes(y=predict(robust_md1.6), group=framing, colour=framing))+
  stat_summary(geom="pointrange", aes(y=predict(robust_md1.6), group=framing, colour=framing)) +
  geom_line(data = other_function, aes(x = trial, y = other, colour= "Fixed participant")) +
  ggtitle("Model (1.6) with random intercepts and robust standard errors fitted to observed data")

#### Plotting interaction
pmd16 <- interact_plot(md1.6,
                             pred=time1,
                             modx= framing, 
                             mod2=av_mbi,
                             cond.int = T,
                             interval = T, 
                             jnplot = T,
                             x.label="Trial",
                             y.label="Contribution (%)",
                         modx.labels=c("Control", "Positive", "Negative"),
                           mod2.label=c("Least emotionally \n exhausted (-1SD)", "Average emotional\n exhaustion", "Most emotionally \n exhausted (+1SD)"),
                         main.title = "Interaction between levels of emotional exhaustion and the effect of valence framing \n over time in model (1.6) with random intercepts")
pmd16
simple_slopes(md1.6)
tab_model(md1.6, show.fstat=T, show.re.var=T, show.stat = T, show.aic=T, title="A table to show results of a LMEM regression of model (1.6) with random intercepts and Gaussian distribution")

######## COMPARING MODEL FIT 
compare_performance(md1.0,md1.1, md1.2, md1.3, md1.4, md1.5, robust_md1.5, md1.6, robust_md1.6)
#model 1.3 and 1.5 are best
anova(md1.3, md1.5)
#1.5 is the best model
sjPlot::tab_model(md1.5)
compare_performance(robust_md1.5, robust_md1.6)
anova(robust_md1.5, robust_md1.6)
anova(md1.5, md1.6)
#massive improvement in fit on every model apart from 4 (I've just removed the interaction and polynomial so that's to be expected)
plot(allEffects(md1.5))
plot_model(md1.5)
VarCorr(md1.5)



################################################### Exploratory Analysis ########################################################
################################## Models I outlined in my pre-registration for exploratory analysis

####################### Exploring demographics
######## MODEL 0
md1.0.1<-lm(average_contribution~framing+ age + gender + income, data=dfsw)
anova(md1.0, md1.0.1)

######## MODEL 2
md1.1.1<-lmer(prop_nonzero~framing + time1 + age + gender + income +(1|id) , data=df)
anova(md1.1, md1.1.1)

######## MODEL 2
md1.2.1<-lmer(prop_nonzero~framing * time1 + age + gender + income + (1|id), data=df)
anova(md1.2, md1.2.1)

######## MODEL 3
md1.3.1<-lmer(prop_nonzero ~ framing *time1 + time2 + lagged_contribution +  age + gender + income+ (1 |id), data = df)
anova(md1.3, md1.3.1)

######## MODEL 4
md1.4.1<-lmer(prop_nonzero~framing * av_mbi + time1 + age + gender + income+ (1|id), data=df)
anova(md1.4, md1.4.1)

######## MODEL 5
md1.5.1<-lmer(prop_nonzero~framing*time1 + av_mbi  + time2 +  lagged_contribution + income + gender + age + (1|id), data=df)
anova(md1.5, md1.5.1)

                               # Including demographics does not improve model fit for models (1.0) to (1.5) #

####################### Including MBI scores
######## MODEL 0
md1.0.2<-lm(average_contribution~framing+ av_mbi, data=dfsw)
anova(md1.0, md1.0.1)

######## MODEL 1
md1.1.2<-lmer(prop_nonzero~framing + time1 + av_mbi +(1|id) , data=df)
anova(md1.1, md1.1.2) #better

######## MODEL 2
md1.2.2<-lmer(prop_nonzero~framing * time1 + av_mbi + (1|id), data=df)
anova(md1.2, md1.2.2) #better

######## MODEL 3 with mbi is just md1.5
compare_performance(md1.0.2, md1.1.2, md1.2.2, md1.4, md1.5) #md1.5 is still best 

            # Including emotional_exhaustion scores improves model fit for models (1.0) to (1.2) #

####################### Random intercepts AND random slopes
######## MODEL 1
md1.1_slopes<-lmer(prop_nonzero~framing + time1 + (1+time1|id), data=df, control = lmerControl(optimizer="bobyqa"))
anova(md1.1, md1.1_slopes)
augment(md1.1_slopes) %>% ggplot(., aes(x=time1,y=.fitted,color=framing)) + stat_summary(geom="line") + stat_summary(geom="pointrange", aes(y=prop_nonzero)) + ggtitle("Model (1.2) with random slopes") 


######## MODEL 2
md1.2_slopes<-lmer(prop_nonzero~framing * time1 + (1+time1|id), data=df, control = lmerControl(optimizer="bobyqa"))
anova(md1.2, md1.2_slopes)
augment(md1.2_slopes) %>% ggplot(., aes(x=time1,y=.fitted,color=framing)) + stat_summary(geom="line") + stat_summary(geom="pointrange", aes(y=prop_nonzero)) + ggtitle("Model (1.2) with random slopes") 
simple_slopes(md1.2_slopes)
p_md1.2_slopes <- probe_interaction(md1.2_slopes,
                                 pred=time1,
                                 modx= framing, 
                                 cond.int = T,
                                 interval = T, 
                                 jnplot = T) 
p_md1.2_slopes
plot_model(md1.2_slopes, type = "int", mdrt.values = "meansd") 
md1.2_slopes_mbi<-lmer(prop_nonzero~framing * time1 + av_mbi+(1+time1|id), data=df, control = lmerControl(optimizer="bobyqa"))
plot_model(md1.2_slopes_mbi, type = "pred", terms = c("time1", "av_mbi [0,2.5, 5]", "framing"))
graph_model(md1.2_slopes_mbi, y=prop_nonzero, x=av_mbi, lines=framing) 
p_md1.2_slopes_mbi <- probe_interaction(md1.2_slopes_mbi,
                                    pred=time1,
                                    modx= framing,
                                    mod2=av_mbi,
                                    cond.int = T,
                                    interval = T, 
                                    jnplot = T) 

######## MODEL 3
md1.3_slopes<-lmer(prop_nonzero~framing * time1 + time2 + lagged_contribution +(1+time1|id), data=df)
anova(md1.3, md1.3_slopes) #better fit
augment(md1.3_slopes) %>% ggplot(., aes(x=time1,y=.fitted,color=framing)) + stat_summary(geom="line") + stat_summary(geom="pointrange", aes(y=prop_nonzero)) + ggtitle("Model (1.3) with random slopes") 

######## MODEL 4
md1.4_slopes<-lmer(prop_nonzero~framing*av_mbi+time1 + (1+time1|id), data=df, control = lmerControl(optimizer="bobyqa"))
anova(md1.4, md1.4_slopes) #better fit
augment(md1.4_slopes) %>% ggplot(., aes(x=time1,y=.fitted,color=framing)) + stat_summary(geom="line") + stat_summary(geom="pointrange", aes(y=prop_nonzero))+ ggtitle("Model (1.4) with random slopes") 
summary(md1.4_slopes)
plot(allEffects(md1.4_slopes))

######## MODEL 5
md1.5_slopes<-lmer(prop_nonzero~framing *time1 + av_mbi + time2 + lagged_contribution + (1+time1|id), data=df, control = lmerControl(optimizer="bobyqa"))
anova(md1.5, md1.5_slopes) #improved fit!
augment(md1.5_slopes) %>% ggplot(., aes(x=time1,y=.fitted,color=framing)) + stat_summary(geom="line") + stat_summary(geom="pointrange", aes(y=prop_nonzero)) + ggtitle("Model (1.5) with random slopes fitted to observed data")+ xlab("Trial") + ylab("Contribution(%)")
summary(md1.5_slopes)
tab_model(md1.5_slopes, show.fstat=T, show.re.var=T, show.aic=T, title="A table to show results of a LMEM regression of model (1.5) with random slopes and Gaussian distribution")
plot(allEffects(md1.5_slopes))
compare_performance(md_null, md1.5_slopes)

                      # Including random slopes improved model fit for models (1.1) to (1.5) #

compare_performance(md1.1_slopes, md1.2_slopes, md1.3_slopes, md1.4_slopes, md1.5_slopes) #model 1.3 and 1.5 are best
anova(md1.3_slopes, md1.5_slopes) #1.5_slopes is best

################################################### Exploratory Analysis #####################################################
################################## Models I did NOT outline in my pre-registration for exploratory analysis

plot_model(md1.5, type = "pred", terms = c("time1", "av_mbi [0,1,2,3,4,5]", "framing"), main="Predicted contribution (%)over time by condition and emotional exhaustion scores")

#####
#Exploring interaction in model 1.5 that wasn't significant'
#exploring interaction terms
plot_model(md1.5_slopes_int, type = "pred", terms = c("framing", "av_mbi[0,1,2,3,4,5]"), main="Predicted contribution (%) by condition and emotional exhaustion scores")
plot_model(md1.5, type = "pred", terms = c("av_mbi[0,1,2,3,4,5]", "framing"), main="Predicted contribution (%) by condition and emotional exhaustion scores")
md_interaction<-lmer(prop_nonzero~ framing *av_mbi*time1 + (1|id), data=df)

p_md1.5 <- probe_interaction(md_interaction, 
                          pred = time1, 
                          modx = framing, 
                          mod2=av_mbi,
                          cond.int = T,
                          interval = T,
                          jnplot = T)

p_md_interaction_slopes <- probe_interaction(md_interaction_slopes, 
                                      pred = time1, 
                                      modx = framing, 
                                      mod2=av_mbi,
                                      cond.int = T,
                                      interval = T,
                                      jnplot = T)
sjPlot::plot_model(md_interaction_slopes, type = "int")
graph_model(md_interaction, y=prop_nonzero, x=av_mbi, lines=framing) 


ggplot(data = df, aes(x=av_mbi, y=prop_nonzero, col=framing))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)
#control is more effective than positive for those who are not burnt out. Those who are burnt out contribute more in the positive condition than in the negative condition

ggplot(data = df, aes(x=time1, y=prop_nonzero, col=av_mbi))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE) +
  facet_wrap(facets = vars(framing))


################################################### Assumption Check ########################################################

###############  Linear relationship?
ggplot(df, aes(time1, contribution_pct, color=framing)) +
  stat_summary(fun.data = mean_se, geom="pointrange") +
  stat_smooth() +
  theme_classic() + xlab("Trial") + ylab("Contribution(%)")+
 geom_line(aes(group = id), alpha = .2) + 
  facet_wrap(~ burntout)


############### Are my errors normally distributed, constant and have a mean of 0?
#relationships
myvars <- names(df.sw) %in% c("id", "bonus", "t_1", "t_2", "t_3", "t_4", "t_5", "t_6", "t_7", "t_8", "t_9", "t_10", "t_11", "t_12", "t_13", "t_14", "t_15","total_contribution", "emotional_exhaustion", "tiredPM", "guilt", "burntout", "hopeless", "change_possible", "care_for_life", "wits_end")
df.sw_subset <- df.sw[!myvars]
pairs.panels(df.sw_subset)

#### Independence of errors, test for autocorrelation in a regression output
durbinWatsonTest(resid(robust_md1.5))

####Plot the residuals vs fitted model, and assess the extend to which the assumption holds that the residuals are zero mean.
plot(robust_md1.5, type=c("p","smooth"))
plot(md1.6_3way, type=c("p","smooth"))

#### Do my residuals have a constant variance?
sjPlot::plot_model(md1.5, type = "diag")
sjPlot::plot_model(md1.6_3way, type = "diag")
augment(md1.5) %>%
  mutate(
    sqrtr = sqrt(abs(.resid))
  ) %>%
  ggplot(aes(x=.fitted, y=sqrtr)) + 
  geom_point() +
  geom_smooth()

#Construct a scale-location plot. This is where the square-root of the absolute value of the standardised residuals is plotted against the fitted values, and allows you to more easily assess the assumption of constant variance.
plot(robust_md1.5,
     form = sqrt(abs(resid(.))) ~ fitted(.),
     type = c("p","smooth"))
plot(md1.6_3way,
     form = sqrt(abs(resid(.))) ~ fitted(.),
     type = c("p","smooth"))

####Examine the normality of both the level 1 and level 2 residuals.
hist(resid(robust_md1.5), main="A histogram to show the distribution of residuals in model (1.5)", xlab= "Residuals")
hist(resid(md1.6_3way), main="A histogram to show the distribution of residuals in model (1.6)", xlab= "Residuals")
qqmath(ranef(md1.6_3way))   #ruh roh
simulateResiduals(alt_beta_md1.3)   #

#The larger the ICC, the lower the variability is within the clusters (relative to the variability between clusters). The greater the correlation between two observations from the same group.
ICCbare(id, contribution_pct, data=df)   #More variability between clusters than within clusters
  
#Test dispersion
testDispersion(robust_md1.5)
testDispersion(md1.6_slopes)

# Random effects
ranef(md1.5)
qqmath(ranef(md1.5))
qqmath(ranef(md1.5_slopes))
rans <- as.data.frame(ranef(md1.5_slopes)$id)
ggplot(rans, aes(sample = `(Intercept)`)) + 
  stat_qq() + stat_qq_line() +
  labs(title="random intercept")
ggplot(rans, aes(sample = time1)) + 
  stat_qq() + stat_qq_line()+
labs(title="random slope")

# check for outliers
infl1 <- hlm_influence(md1.5, level = "id")
dotplot_diag(infl1$cooksd, cutoff = "internal")

# Check for correlations between random effects
VarCorr(md1.5)

shapiro.test(resid(robust_md1.5))
r_int<- ranef(md1.5)$id$`(Intercept)`
r_int_slopes<- ranef(md1.5_slopes)$id$`(Intercept)`
r_slopes<- ranef(md1.5_slopes)$id$time1
shapiro.test(r_int)
shapiro.test(r_slopes)
shapiro.test(r_int_slopes)


rand(md1.5)
VarCorr(robust_md1.5)

library(merTools)
plotREsim(REsim(md1.5))
