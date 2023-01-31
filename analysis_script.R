# analysis script
# this script is used to carry out the main analyses
# It relies on other scripts to prepare the data
# But we cannot provide access to the raw data, so these are the summarized data here


# packages
library(dplyr)
library(ggplot2)

# read in some of the summary files
# These are the summary of how people sample in response to
# the experimental groups
# So if people are actually 'following' the nudges then there would be more samples
# in high and medium priority cells...
# 'total' is just the total summary
# and 'temporal sampling' is repeated measures - one for each 2 week period
# ignore the various plotting stuff. I'll make all the plots look consistent for paper.
total <- readRDS("total_percentage_samples_per_square.RDS")

temporal_sampling <- readRDS("temporal_percentage_samples_per_square.RDS")

temporal_sampling_cumulative <- readRDS("temporal_percentage_samples_per_square_cumulative.RDS")

# make plot of 'total'
## For positioning of geom_rect 
total$xpos <- c(3,3,3,3,3,5,5,5,5,5,6,6,6,6,6,4,4,4,4,4,1,1,1,1,1,2,2,2,2,2)
total <- arrange(total, xpos)
total$lga <- factor(total$lga, levels=c("Lake Macquarie (C)", "Wingecarribee (A)", "Central Coast (C) (NSW)", "Wollongong (C)","Hornsby (A)", "Blue Mountains (C)"))

colour_scale <- c("orange","yellow","green","#808080", "#000A73", "#0016FF", "#006EFF", "#9EC8FF")

#   scale_fill_manual(values = experimental_group_colour_scale, breaks=c("Experimental", "Experimental with Leaderboard", "Control"), name="Experimental Group") + 

ggplot(total) + 
  geom_bar(aes(fill=as.factor(status), y=percentage*100, x=lga), position="stack", stat="identity", alpha=1) +
  #geom_rect(aes(xmin = xpos - .5, xmax = xpos + 0.5, ymin=-Inf, ymax=Inf, fill=as.factor(experimental_group)), color=NA, alpha = 0.02) +
  ylab("Percentage (%)") + 
  xlab("Local Government Area") + 
  scale_fill_manual(values = colour_scale, breaks=c("Experimental", "Experimental with Leaderboard", "Control","zero records", "insufficient records", "high priority", "medium priority", "low priority"), name="Status") + 
  scale_color_manual(values = colour_scale, breaks=c("zero records", "insufficient records", "high priority", "medium priority", "low priority"), name="Status") +
  #ggtitle(paste0("Proportion of samples per square from 28/09/20 to 02/10/2021")) + 
  ylim(0, 101) + 
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme(text = element_text(colour = "white"))+
  theme(axis.text=element_text(colour="white"))+
  theme(axis.ticks=element_line(colour="white"))+
  theme(axis.line=element_line(colour="white"))+
  theme(plot.background=element_rect(fill="black"))+
  theme(panel.background=element_rect(fill="black"))+
  theme(legend.background=element_rect(fill="black"))

total %>%
  mutate(experimental_group=case_when(experimental_group=="Control" ~ "control",
                                      experimental_group=="Experimental" ~ "dynamic map",
                                      experimental_group=="Experimental with Leaderboard" ~ "dynamic map + leaderboard")) %>%
  mutate(status=factor(status, levels=c("zero records", "insufficient records", "high priority",
                                           "medium priority", "low priority"))) %>%
  mutate(lga_name=case_when(lga=="Lake Macquarie (C)" ~ "Lake Macquarie",
                            lga=="Wingecarribee (A)" ~ "Wingecarribee",
                            lga=="Central Coast (C) (NSW)" ~ "Central Coast",
                            lga=="Wollongong (C)" ~ "Wollongong",
                            lga=="Hornsby (A)" ~ "Hornsby",
                            lga=="Blue Mountains (C)" ~ "Blue Mountains")) %>%
  mutate(lga_name=factor(lga_name, 
                         levels=c("Lake Macquarie", "Wingecarribee", "Central Coast", "Wollongong", "Hornsby", "Blue Mountains"))) %>%
  ggplot(.) + 
  geom_bar(aes(fill=status, y=percentage*100, x=lga_name), 
           position="stack", stat="identity", alpha=1)+
  ylab("Percentage of cells sampled") + 
  xlab("Study region")+ 
  scale_fill_manual(values = c("gray30", "gray80", "#00AFBB", "#E7B800", "#FC4E07"), 
                    breaks=c("zero records", "insufficient records", "high priority", 
                             "medium priority", "low priority"), 
                    name="Sampling priority status") + 
  scale_color_manual(values = colour_scale, breaks=c("zero records", "insufficient records", "high priority", 
                                                     "medium priority", "low priority"), name="Status") +
  ylim(0, 105) + 
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme(axis.text=element_text(color="black"))+
  theme(panel.grid=element_blank())+
  geom_label(x=1.5, y=5, label="control", size=3)+
  geom_label(x=3.5, y=5, label="dynamic map", size=3)+
  geom_label(x=5.5, y=5, label="dynamic map + leaderboard", size=3)


# need to statistically test this 2 ways
# 1) "Experimental (grouping both types)" vs "Control"
# 2) Experimental vs control vs experimental with leaderboard

library(tidyverse)

#each region is split into cells with different priority status
subset(total,lga=="Hornsby (A)")
subset(total,lga=="Central Coast (C) (NSW)")
table(total$lga,total$experimental_group)

#set order
total$experimental_group <- factor(total$experimental_group,
                                   levels=c("Control","Experimental",
                                            "Experimental with Leaderboard"))

total$status<- factor(total$status,
                      levels=c("zero records","insufficient records",
                               "low priority","medium priority","high priority"))


#as mixed model - include lga as random effect to account for clustering of data
library(lme4)

#standard treatment contrasts
contrasts(total$experimental_group) = contr.treatment(3, base=1)
contrasts(total$experimental_group)

#overall effect
glmer1 <- glmer(no_samples ~ experimental_group + (1|lga), 
                offset = log(no_cells+1), data = total, family = "poisson")

#pull out status-specific effects
glmer1 <- glmer(no_samples ~ status + experimental_group:status + (1|lga), 
                offset = log(no_cells+1), data = total, family = "poisson")

summary(glmer1)
#Wald are quicker but we shoudld switch later
coefs <- confint(glmer1, method='Wald') %>%
  as.data.frame() %>%
  add_column(Param = row.names(.)) %>%
  filter(Param!=".sig01") %>%
  add_column(Estimate = summary(glmer1)$coef[,"Estimate"]) %>%
  filter(grepl(":",Param)) %>%
  separate(Param,c("Level","Group"),sep=":") %>%        
  janitor::clean_names() %>%        
  mutate(group= gsub("experimental_group","",group)) %>%
  mutate(level = as.factor(gsub("status","",level)))  %>%
  mutate(level = factor(level, levels=c("zero records","insufficient records",
                                        "low priority","medium priority",
                                        "high priority"))) %>%
  mutate(group = ifelse(group==2,"Experimental",
                        "Experiment with leaderboard)"))

ggplot(coefs)+
  geom_pointrange(aes(x=level, y=estimate, 
                      ymin=x2_5_percent, ymax=x97_5_percent,colour=group))+
  geom_hline(yintercept=0)+
  scale_color_viridis_d("Compared to control") +
  theme_classic()+
  ylab("Difference in sampling rate")+
  coord_flip() 

#hypothesis tests

levels(total$experimental_group)
contrasts(total$experimental_group) <- matrix(c(-2, 1, 1, 0, -0.5, 0.5), ncol = 2)
glmer1 <- glmer(no_samples ~ status + experimental_group:status + (1|lga), 
                offset = log(no_cells+1), data = total, family = "poisson")

coefs <- confint(glmer1, method='Wald') %>%
  as.data.frame() %>%
  add_column(Param = row.names(.)) %>%
  filter(Param!=".sig01") %>%
  add_column(Estimate = summary(glmer1)$coef[,"Estimate"]) %>%
  filter(grepl(":",Param)) %>%
  separate(Param,c("Level","Group"),sep=":") %>%        
  janitor::clean_names() %>%        
  mutate(group= gsub("experimental_group","",group)) %>%
  mutate(level = as.factor(gsub("status","",level)))  %>%
  mutate(level = factor(level, levels=c("zero records","insufficient records",
                                        "low priority","medium priority",
                                        "high priority"))) %>%
  mutate(group = ifelse(group==1,"experiment vs control","with vs without leaderboard")) %>%
  mutate(group=case_when(group=="experiment vs control" ~ "dynamic map vs control",
                         group=="with vs without leaderboard" ~ "dynamic map + leaderboard vs dynamic map")) %>%
  mutate(group=factor(group, levels=c("dynamic map vs control", "dynamic map + leaderboard vs dynamic map")))

ggplot(coefs)+
  geom_pointrange(aes(x=level, y=estimate, 
                      ymin=x2_5_percent, ymax=x97_5_percent))+
  geom_hline(yintercept=0)+
  theme_bw()+
  ylab("Effect of experiment")+
  xlab("Sampling priority status")+
  theme(axis.text=element_text(color="black"))+
  theme(panel.grid=element_blank())+
  coord_flip()+
  facet_wrap(~group, ncol=1)

#############
############
#############
####### Now in addition (or maybe even instead of)
# can run this where each sampling period is a 'random effect'?
# make a figure

colour_scale <- c("orange","yellow","green","#808080", "#000A73", "#0016FF", "#006EFF", "#9EC8FF")

ggplot(temporal_sampling_cumulative, aes(fill=status, color=status, y=percentage, x=sampling_period)) +
  geom_bar(position="stack", stat="identity", alpha=0.5) +
  ylab("Percentage of cells sampled") +
  xlab("Sampling period") +
  scale_fill_manual(values = colour_scale, breaks = c("zero records", "insufficient records", "high priority", "medium priority", "low priority")) +
  scale_color_manual(values = colour_scale, breaks=c("zero records", "insufficient records", "high priority", "medium priority", "low priority")) +
  ylim(0, 1.1) +
  theme_bw()+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  facet_wrap(~lga, ncol=1)+
  theme(axis.text.x=element_text(angle=90))

ggplot(temporal_sampling %>%
         mutate(lga=factor(lga, levels=c("Lake Macquarie (C)", "Wingecarribee (A)",
                                            "Central Coast (C) (NSW)", "Wollongong (C)",
                                            "Hornsby (A)", "Blue Mountains (C)"))))+
  geom_bar(aes(fill=as.factor(status), y=percentage*100, x=sampling_period), 
           position="stack", stat="identity", alpha=1)+
  ylab("Percentage of cells sampled") + 
  xlab("Sampling period")+ 
  scale_fill_manual(values = colour_scale, 
                    breaks=c("Experimental", "Experimental with Leaderboard", "Control",
                             "zero records", "insufficient records", "high priority", 
                             "medium priority", "low priority"), 
                    name="Status") + 
  scale_color_manual(values = colour_scale, breaks=c("zero records", "insufficient records", "high priority", 
                                                     "medium priority", "low priority"), name="Status") +
  ylim(0, 105)+
  theme_bw()+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=90))+
  theme(axis.text=element_text(color="black"))+



# spatial bias data
# this file is the summary of the 'spatial bias'
# in sampling.
# This was kind of tricky because it isn't just about spatial bias per se
# as some spatial bias in the sampling is expected
# so this calculates the delta between a null model of spatial bias given our sampling priorities
# and the observed spatial bias given the sampling priorities given to the participants
# And it compares 'before' and 'after' where before is randomly sampled
# in other words we woudl expect the 'delta' to be less after the experiment
# if people are following the sampling priorities to some extent
spatial_bias <- readRDS("spatial_bias_data.RDS") %>%
  mutate(experimental_group2=case_when(experimental_group=="Control" ~ "control",
                                      experimental_group=="Experimental" ~ "dynamic map",
                                      experimental_group=="Experimental with Leaderboard" ~ "dynamic map + leaderboard"))

color_scale <- c("yellow", "green", "orange")

# here is an example of the plot to make for this
ggplot(spatial_bias, aes(x=type, y=delta)) +
  geom_boxplot() +
  coord_flip() +
  facet_grid(rows=vars(lga)) + 
  theme_bw() +
  theme(axis.text=element_text(color="black"))+
  geom_rect(data = spatial_bias, aes(fill=as.factor(experimental_group2)), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.01) +
  scale_fill_manual(values=color_scale, breaks=c("dynamic map", "dynamic map + leaderboard", "control"), name="") + 
  ylab("Delta mean square distances (m)") +
  xlab("")+
  guides(fill = guide_legend(override.aes = list(alpha = 0.5)))+
  theme(strip.text=element_text(size=8))

spatial_bias$experimental_group <- factor(spatial_bias$experimental_group,
                                          levels=c("Control","Experimental",
                                                   "Experimental with Leaderboard"))

levels(spatial_bias$experimental_group)
contrasts(spatial_bias$experimental_group) <- matrix(c(-2, 1, 1, 0, -0.5, 0.5), ncol = 2)
glmer1 <- glmer(delta ~ type + experimental_group:type + (1|lga), data = spatial_bias, family = "gaussian")

coefs <- confint(glmer1, method='Wald') %>%
  as.data.frame() %>%
  add_column(Param = row.names(.)) %>%
  filter(Param!=".sig01") %>%
  filter(Param!=".sigma") %>%
  add_column(Estimate = summary(glmer1)$coef[,"Estimate"]) %>%
  filter(grepl(":",Param)) %>%
  separate(Param,c("Level","Group"),sep=":") %>%        
  janitor::clean_names() %>%        
  mutate(group= gsub("experimental_group","",group)) %>%
  mutate(level = as.factor(gsub("type","",level)))  %>%
  mutate(group = ifelse(group==1,"experiment vs control","with vs without leaderboard")) %>%
  mutate(group=case_when(group=="experiment vs control" ~ "dynamic map vs control",
                         group=="with vs without leaderboard" ~ "dynamic map vs dynamic map + leaderboard"))

ggplot(coefs)+
  geom_pointrange(aes(x=level, y=estimate, 
                      ymin=x2_5_percent, ymax=x97_5_percent))+
  geom_hline(yintercept=0)+
  theme_bw()+
  ylab("Effect of experiment")+
  xlab("Before/after experiment")+
  theme(axis.text=element_text(color="black"))+
  theme(panel.grid=element_blank())+
  coord_flip()+
  facet_wrap(~group, ncol=1)




