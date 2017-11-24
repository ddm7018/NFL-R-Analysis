library(devtools)
#devtools::install_github(repo = "maksimhorowitz/nflscrapR", force=TRUE)

library(RColorBrewer)
library(nflscrapR)
library(ggplot2)
library(dplyr)
library(pheatmap)
library(ggjoy)
library(d3heatmap)
library(reshape2)
library(htmlwidgets)
pbp_2017 <- season_play_by_play(2017)


min_rush_cnt <- 150
rush_cnt <- pbp_2017 %>% filter(PlayType == 'Run') %>%
  group_by(Rusher) %>% 
  summarise(rush_cnt = n(),
            total_yards = sum(Yards.Gained),
            mean_yards = round(mean(Yards.Gained), 2)) %>%
  filter(rush_cnt >= min_rush_cnt) %>%
  arrange(desc(rush_cnt))


rushing_stats <- pbp_2017 %>%
  filter(PlayType == 'Run' & Rusher %in% rush_cnt$Rusher & Yards.Gained <=50) %>%
  filter(down!=4 & !is.na(down)) %>%
  filter(!is.na(RunLocation))

ggplot(rushing_stats, aes(x = Yards.Gained, y = Rusher, fill=Rusher)) +
  geom_joy(scale = 3) +
  theme_joy() +
  scale_fill_manual(values=rep(c('gray', 'lightblue'), length(rushing_stats$Rusher)/2)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position="none") +
  labs(x="Yards gained per play" ,y="")

usage_stats <- pbp_2017 %>% filter(!is.na(down) & Rusher %in% rush_cnt$Rusher & qtr!=5) %>%
  group_by(Rusher, down, qtr) %>%
  summarise(cnt = n()) %>%
  mutate(qtr_down = paste("Q", qtr, "- Down: ", down, sep=""))

usage <- usage_stats %>% dcast(Rusher ~ qtr_down, value.var = "cnt")

# clean data

row.names(usage) <- usage$Rusher
usage <- usage %>% select(-Rusher)
usage[is.na(usage)] <- 0

# normalize data

usage_norm <- t(apply(usage, 1, function(x) x/sum(x)))

# Plot heatmap of proportions of rushes by different field locations and gaps

p <- d3heatmap(usage_norm,
               colors="Blues",
               Colv=FALSE,
               show_grid=3)
p
saveWidget(p, file="rusher_usage_down_quarter.html")
ggplot(rushing_stats, aes(x = Yards.Gained, y = down)) +
  geom_joy(scale=1, rel_min_height=.03, fill='black') +
  scale_y_discrete(expand = c(0.01, 0)) +
  xlab('Value') +
  facet_wrap(~Rusher, scales='free', ncol=3) +
  theme_joy() +
  theme(axis.title.y = element_blank())+
  labs(x="Yards gained per play" ,y="Down")

ggplot(data=rushing_stats, aes(x=RunLocation, y=Yards.Gained, color=RunLocation)) +
  geom_jitter(position=position_jitter(0.2)) +
  stat_summary(fun.data=mean_sdl, mult=1, 
               geom="pointrange", color="black") +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  facet_wrap(~Rusher)

rush_locations <- rushing_stats %>% filter(PlayType=='Run') %>%
  filter(!is.na(RunLocation)) %>%
  group_by(Rusher, RunLocation) %>%
  summarise(rush_cnt = n()) %>%
  mutate(freq = rush_cnt / sum(rush_cnt))

loc_mat <- rush_locations %>% dcast(Rusher ~ RunLocation, value.var = "freq")
row.names(loc_mat) <- loc_mat$Rusher
loc_mat <- loc_mat %>% select(-Rusher)

pheatmap(loc_mat, border="white", color = brewer.pal(9,"Blues"), cluster_cols=FALSE)

ggplot(data=rushing_stats %>% filter(!is.na(RunGap)), aes(x=RunGap, y=Yards.Gained, color=RunGap)) +
  geom_jitter(position=position_jitter(0.2)) +
  stat_summary(fun.data=mean_sdl, mult=1, 
               geom="pointrange", color="black") +
  scale_color_brewer(palette="Dark2") + theme_minimal() +
  facet_wrap(~Rusher)


rush_gaps <- rushing_stats %>% filter(!is.na(RunGap)) %>%
  filter(!is.na(RunGap)) %>%
  group_by(Rusher, RunGap) %>%
  summarise(rush_cnt = n()) %>%
  mutate(freq = rush_cnt / sum(rush_cnt))

gap_mat <- rush_gaps %>% dcast(Rusher ~ RunGap, value.var = "freq")
row.names(gap_mat) <- gap_mat$Rusher
gap_mat <- gap_mat %>% select(-Rusher)
# Plot heatmap of proportions of rushes by different field gaps

pheatmap(gap_mat, border="white", color = brewer.pal(9,"Blues"), cluster_cols=FALSE)
