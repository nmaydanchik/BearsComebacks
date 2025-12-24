library(tidyverse)
library(patchwork)
library(stringr)

load("BearsCompare.RData")
offense_compare <- as.data.frame(cbind(sample=rownames(offense_compare), offense_compare), row.names = FALSE);
defense_compare <- as.data.frame(cbind(sample=rownames(defense_compare), defense_compare), row.names = FALSE); defense_compare[,1]=c("All quarters","All quarters excluding fourth\nquarters of comebacks", "Fourth quarters\nof comebacks", "First three quarters\nof comeback games", "All fourth quarters", "Fourth quarters of one possesion losses")
special_teams_compare <- as.data.frame(cbind(sample=rownames(special_teams_compare), special_teams_compare), row.names = FALSE); special_teams_compare[,1]=c("All quarters","All quarters excluding fourth\nquarters of comebacks", "Fourth quarters\nof comebacks", "First three quarters\nof comeback games", "All fourth quarters", "Fourth quarters of one possesion losses")

# Epa/play_type/sample

offense <- offense_compare[c(1,2,3), ] %>% select(sample, epa, epa_pass, epa_run, epa_dropback); offense[,1]=c("All quarters","All quarters excluding fourth\nquarters of comebacks","Fourth quarters of comebacks"); colnames(offense)[2:5]=c("epa_All plays", "epa_Pass", "epa_Run", "epa_Dropbacks")
offense[,2:5] <- sapply(offense[,2:5], as.numeric)
offense$sample <- factor(offense$sample, levels=c("All quarters","All quarters excluding fourth\nquarters of comebacks","Fourth quarters of comebacks"))

olong <- pivot_longer(offense, names_to = "play_type", names_prefix="epa_",cols=starts_with("epa_"), values_to="epa")

mycols <- RColorBrewer::brewer.pal(8, "RdYlBu")[c(8:7, 2)]
olong %>% ggplot(mapping=aes(x=sample, y=epa, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  facet_wrap(~play_type, nrow=1, strip.position="bottom") +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  geom_hline(yintercept=0) +
  labs(title="Bears offensive efficiency skyrockets during the fourth quarters of comebacks", x="", y="EPA per play") +
  guides(fill=guide_legend(title="Quarters"))
ggsave("OffenseEPA_5.png", width=8, height=5, dpi=600)  

# Big/bad play rate/sample

offense <- offense_compare[c(2,3), ] %>% select(sample, big_play_rate, bad_play_rate); offense[,1]=c("All quarters excluding\nfourth quarters of comebacks","Fourth quarters\nof comebacks")
offense[,2:3] <- sapply(offense[,2:3], as.numeric)
offense$sample <- factor(offense$sample, levels=c("All quarters excluding\nfourth quarters of comebacks","Fourth quarters\nof comebacks"))

olong <- pivot_longer(offense, names_to = "play_type", names_pattern="(.*)_play_rate",cols=contains("_rate"), values_to="rate")
olong$play_type <- factor(olong$play_type, c("big", "bad"))

olong %>% ggplot(mapping=aes(x=play_type, y=rate, fill=-rate)) +
  geom_col(position=position_dodge(width=0.8)) +
  facet_wrap(~sample, nrow=1) +
#  scale_fill_manual(values=mycols) + +
  coord_cartesian(ylim=c(0.16,0.24)) +
#  scale_y_continuous(limits=c(0,0.))
  ggthemes::theme_solarized_2() +
  theme(legend.position = "none") +
  labs(x="", y="Rate", title = "Bears offense makes more big plays and less bad plays in comebacks",
       subtitle="Big play rate = percentage of plays with EPA > 0.90\nBad play rate = percentage of plays with EPA < -0.75")
ggsave("OffenseBigPlays_2.png", width=8, height=5, dpi=600)  

# Explosive play rate

offense <- offense_compare[c(2,3), ] %>% select(sample, explosive_play_rate); offense[,1]=c("All quarters excluding\nfourth quarters of comebacks","Fourth quarters of comebacks")
offense[,2] <- sapply(offense[,2], as.numeric)
offense$sample <- factor(offense$sample, levels=c("All quarters excluding\nfourth quarters of comebacks","Fourth quarters of comebacks"))

mycols <- RColorBrewer::brewer.pal(8, "RdYlBu")[c(8, 2)]
offense %>% ggplot(mapping=aes(x=sample, y=explosive_play_rate, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  scale_fill_manual(values=mycols) +
  coord_cartesian(ylim=c(0,0.2)) +
  ggthemes::theme_solarized_2() +
  theme(legend.position = "none") +
  labs(x="", y="Explosive play rate", title = "Bears offense explosive play rate nearly doubles during fourth quarters of comebacks",
       subtitle="Explosive play defined as 20+ yard pass or 10+ yard run")
ggsave("OffenseExplosivePlays_3.png", width=9, height=6, dpi=600) 

# TO rate

offense_compare <- offense_compare[c(1,2,3,4), ] %>% select(sample, to_rate); offense_compare[,1]=c("season","control","comebacks","q1-q3")
offense_compare[,2] <- sapply(offense_compare[,2], as.numeric)
offense_compare$sample <- factor(offense_compare$sample, levels=c("season","control","q1-q3","comebacks"))

mycols <- RColorBrewer::brewer.pal(8, "RdYlBu")[c(8,8,8, 2)]
offense_compare %>% ggplot(mapping=aes(x=sample, y=to_rate, fill=sample)) +
  geom_col(position=position_dodge(width=0.8), color="black") +
  scale_fill_manual(values=mycols) +
#  coord_cartesian(ylim=c(0,0.2)) +
  ggthemes::theme_solarized_2() +
  theme(legend.position = "none")

# What it's not: completion percentage

offense <- offense_compare[c(1,2,3,4,5), ] %>% select(sample, completion_percent); offense[,1]=c("All quarters","All quarters excluding fourth\nquarters of comebacks","Fourth quarters\nof comebacks", "First three quarters\nof comeback games","All fourth quarters")
offense[,2] <- as.numeric(offense[,2])
offense$sample <- factor(offense$sample, levels=c("All quarters","All quarters excluding fourth\nquarters of comebacks", "First three quarters\nof comeback games", "Fourth quarters\nof comebacks","All fourth quarters"))

mycols <- RColorBrewer::brewer.pal(8, "RdYlBu")[c(8,8,8,2,8)]
offense %>% ggplot(mapping=aes(x=sample, y=completion_percent, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  scale_fill_manual(values=mycols) +
  coord_cartesian(ylim=c(0.5,0.6)) +
  ggthemes::theme_solarized_2() +
  theme(legend.position = "none") +
  labs(x="", y="Completion percentage", title="Caleb Williams is not completing more passes in comebacks")
ggsave("OffenseCompPct_1.png", width=9, height=6, dpi=600)

# What it's not: Defense EPA, Turnovers, and explosive plays

defense <- defense_compare[c(2,3,4), ] %>% select(sample, epa, to_rate, explosive_play_rate)
defense[,2:4] <- sapply(defense[,2:4], as.numeric)
# defense$sample <- factor(defense$sample, levels=defense$sample[c(1,2)])

mycols <- RColorBrewer::brewer.pal(8, "RdYlBu")[c(8,7, 2)]
p1 <- defense %>% ggplot(mapping=aes(x=sample, y=epa, fill=sample)) +
  geom_col(position = position_dodge(width=0.8)) +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  labs(y="EPA per play", x="", title="Bears defense is playing worse during comebacks", subtitle="Opposing offense\nEPA per play") + 
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  coord_cartesian(ylim=c(-0.03,0.01)) +
  geom_hline(yintercept=0) +
  theme(legend.position = "none")

p2 <- defense %>% ggplot(mapping=aes(x=sample, y=to_rate, fill=sample)) +
  geom_col(position = position_dodge(width=0.8)) +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  coord_cartesian(ylim=c(0.00, 0.04)) +
  labs(x="", y="Takeaway rate", subtitle="Bears defensive\ntakeaway rate") +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  geom_hline(yintercept=0) +
  guides(fill=guide_legend(title="Quarters"))

p3 <- defense %>% ggplot(mapping=aes(x=sample, y=explosive_play_rate, fill=sample)) +
  geom_col(position = position_dodge(width=0.8)) +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  coord_cartesian(ylim=c(0.00, 0.16)) +
  scale_y_continuous(breaks=c(0,0.04,0.08,0.12,0.16)) +
  labs(x="", y="Explosive play rate", subtitle="Opposing offense\nexplosive play rate") +
  theme(legend.position = "none") +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  geom_hline(yintercept=0)
p1+p3+p2
ggsave("DefenseSummary_6.png", width=8, height=5, dpi=600)

# Special teams big play rate

special_teams <- special_teams_compare[c(2,3), ] %>% select(sample, big_play_rate)
special_teams[,2] <- as.numeric(special_teams[,2])
# special_teams_compare$sample <- factor(special_teams_compare$sample, levels=c("control", "q1-q3", "opl", "comebacks"))

mycols <- RColorBrewer::brewer.pal(8, "RdYlBu")[c(8, 2)]
special_teams %>% ggplot(mapping=aes(x=sample, y=big_play_rate, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  scale_fill_manual(values=mycols) +
  coord_cartesian(ylim=c(0,0.15)) +
  ggthemes::theme_solarized_2() +
  theme(legend.position = "none") +
  labs(x="", y="Big play rate", title="Bears special teams big play rate nearly doubles during fourth quarters of comebacks",
       subtitle="Big play rate = percentage of plays with EPA > 0.90")
ggsave("SpecialTeams_1.png", width=9, height=6, dpi=600) 

# Offensive improvement not good enough

offense <- offense_compare[c(3,4,6), ] %>% select(sample, epa, big_play_rate, bad_play_rate, explosive_play_rate, to_rate, completion_percent, yac); offense[,1]=c("Fourth quarters\nof comebacks", "First three quarters\nof comeback games", "Fourth quarters of\none possesion losses")
offense[,2:8] <- sapply(offense[,2:8], as.numeric)
offense$sample <- factor(offense$sample, levels=c("First three quarters\nof comeback games", "Fourth quarters of\none possesion losses", "Fourth quarters\nof comebacks"))

mycols <- RColorBrewer::brewer.pal(8, "RdYlBu")[c(6,1,2)]
p1 <- offense %>% ggplot(mapping=aes(x=sample, y=epa, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim=c(0,0.3)) +
  labs(subtitle="EPA per play", y="", x="", title="Bears offensive efficiency does not improve enough in fourth quarters\nof one possesion losses") +
  theme(legend.position = "none")

p2 <- offense %>% ggplot(mapping=aes(x=sample, y=explosive_play_rate, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim=c(0,0.2)) +
  labs(x="", y="", subtitle="Explosive play rate") +
  guides(fill=guide_legend(title="Quarters"))

p3 <- offense %>% ggplot(mapping=aes(x=sample, y=completion_percent, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim=c(.4,0.6)) +
  labs(title="Caleb Williams makes less passes, receivers get less yards after the\ncatch in fourth quarters of one possesion losses", x="", subtitle ="Completion percentage", y="") +
  theme(legend.position = "none")

p4 <- offense %>% ggplot(mapping=aes(x=sample, y=yac, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim=c(4,8)) +
  labs(x="", y="", subtitle ="Yards after the catch") +
  theme(legend.position = "none")

(p1+p2)/(p3+p4)
ggsave("BadOffense_2.png", width=9, height=6, dpi=600) 

# Big/bad play rate not good enough

olong <- offense %>% select(sample, big_play_rate, bad_play_rate) %>% pivot_longer(names_to = "play_type", names_pattern="(.*)_play_rate",cols=contains("_rate"), values_to="rate")
olong$play_type <- factor(olong$play_type, c("big", "bad"))

olong %>% ggplot(mapping=aes(x=play_type, y=rate, fill=-rate)) +
  geom_col(position=position_dodge(width=0.8)) +
  facet_wrap(~sample, nrow=1) +
  #  scale_fill_manual(values=mycols) + +
  coord_cartesian(ylim=c(0.16,0.24)) +
  #  scale_y_continuous(limits=c(0,0.))
  ggthemes::theme_solarized_2() +
  theme(legend.position = "none") +
  labs(x="", y="Rate", title = "Bears offense makes too many bad plays in one possesion losses",
       subtitle="Big play rate = percentage of plays with EPA > 0.90\nBad play rate = percentage of plays with EPA < -0.75")
ggsave("BigPlayRateLosses.png", width=9, height=6, dpi=600) 

