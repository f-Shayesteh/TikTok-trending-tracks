library(GGally)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(ggwordcloud)
library(tidyverse)
library(caret)
library(asbio)
library(olsrr)
library(xtable)
library(shiny)
library(knitr)
library(DT)
library(scatterplot3d)
library(Hmisc)
library(rgl)
library(faraway)
########################

TikTok = read.csv("C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/STAT840/Final project/Proposal/tiktok1.csv")
TikTok.data = read.csv("C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/STAT840/Final project/Proposal/tiktok.csv")
######################
head(TikTok)

###Recode
TikTok.data$mode_f = is.factor(TikTok.data$mode)
TikTok.data$mode_f <- factor(TikTok.data$mode, labels = c("minor", "major"))


ggpairs(TikTok.data, cardinality_threshold = NULL)

# on the small dataset
ggpairs.plot <- TikTok %>% 
  GGally::ggpairs(title = "Matrix of plots - ggpairs")

class(ggpairs.plot)

ggsave(filename = "G:/mihaela/TEACHING/04_ggpairs_diamonds.png", 
       plot = ggpairs.plot,
       units = "cm", width = 29.7, height = 21, dpi = 600) 


# word cloud for popularity
# smaller dataset
TikTok.small <- TikTok %>% dplyr::sample_n(size = 500, replace = F)

word.cloud = TikTok.small %>% 
  ggplot(aes(label = TikTok.small$artist_name, 
             size = TikTok.small$popularity, 
             color = TikTok.small$artist_name)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 30) +
  scale_color_viridis_d(option = "magma") +
  theme_minimal()
word.cloud
ggsave(filename = "C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/DATA 824/Final Project/figures/wordcloud.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)

#### Data exploratory
summary(TikTok.data)
TikTok.data %>% 
  ggplot(aes(x = popularity)) +
  geom_density(adjust = 1/5, color = "black", fill = "deepskyblue3") +
  xlab("TikTok tracks' popularity") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0,100,20)) +
  ggtitle("TikTok popularity - density plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"))

ggsave(filename = "C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/DATA 824/Final Project/figures/popularity.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)

###The plot shows that only a small number of tracks have the popularity score greater than 90. 

TikTok.data %>% 
  ggplot(aes(x = danceability)) +
  geom_density(adjust = 1/5, color = "black", fill = "deepskyblue3") +
  xlab("TikTok tracks' danceability") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0.15,1,0.2)) +
  ggtitle("TikTok danceability - density plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"))
ggsave(filename = "C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/DATA 824/Final Project/figures/danceability.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)

### The plot shows that only the small number of tracks are not  suitable for dancing.
TikTok.data %>% 
  ggplot(aes(x = loudness)) +
  geom_density(adjust = 1/5, color = "black", fill = "deepskyblue3") +
  xlab("TikTok tracks' loudness") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(-27,1.5,5)) +
  ggtitle("TikTok loudness - density plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"))
ggsave(filename = "C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/DATA 824/Final Project/figures/loudness.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)
###The plot shows that the majority of tracks' loudness is between -10 and -2

TikTok.data %>% 
  ggplot(aes(x = speechiness)) +
  geom_density(adjust = 1/5, color = "black", fill = "deepskyblue3") +
  xlab("TikTok tracks' speechiness") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0,1,0.15)) +
  ggtitle("TikTok speechiness - density plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"))
ggsave(filename = "C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/DATA 824/Final Project/figures/speechiness.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)
###The plot shows that the majority of tracks don't have the spoken words.

TikTok.data %>% 
  ggplot(aes(x = acousticness)) +
  geom_density(adjust = 1/5, color = "black", fill = "deepskyblue3") +
  xlab("TikTok tracks' acousticness") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0,1,0.2)) +
  ggtitle("TikTok acousticness - density plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"))
ggsave(filename = "C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/DATA 824/Final Project/figures/acousticness.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)
###It shows the small number of tracks are acoustic. 
TikTok.data %>% 
  ggplot(aes(x = liveness)) +
  geom_density(adjust = 1/5, color = "black", fill = "deepskyblue3") +
  xlab("TikTok tracks' liveness") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0,1,0.15)) +
  ggtitle("TikTok liveness - density plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"))
ggsave(filename = "C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/DATA 824/Final Project/figures/liveness.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)

##The plot shows that most tracks lack the presence of an audience in the recording.
##valence

TikTok.data %>% 
  ggplot(aes(x = valence)) +
  geom_density(adjust = 1/5, color = "black", fill = "deepskyblue3") +
  xlab("TikTok tracks' valence") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0,1,0.2)) +
  ggtitle("TikTok valence - density plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"))
ggsave(filename = "C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/DATA 824/Final Project/figures/valence.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)


###The plot shows that only a small number of tracks have the valence score less than 0.2.
###tempo
TikTok.data %>% 
  ggplot(aes(x = tempo)) +
  geom_density(adjust = 1/5, color = "black", fill = "deepskyblue3") +
  xlab("TikTok tracks' tempo") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0,250,50)) +
  ggtitle("TikTok tempo - density plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"))
ggsave(filename = "C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/DATA 824/Final Project/figures/tempo.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)

##The plot shows that the majority of popular tracks have the tempo score between 100-150.
###duration

TikTok.data %>% 
  ggplot(aes(x = duration_mins)) +
  geom_density(adjust = 1/5, color = "black", fill = "deepskyblue3") +
  xlab("TikTok tracks' duration (min)") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0,15,3)) +
  ggtitle("TikTok duration (min) - density plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"))
ggsave(filename = "C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/DATA 824/Final Project/figures/duration.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)

###The plot shows that the majority of tracks are about 3 minute long. Only a afew tracks are longer than 7 minutes. 
##### plot tableplot using tabplot function

library(devtools)
install_github("mtennekes/tabplot")
library(tabplot)
tableplot(TikTok.data,
          
          select = c(popularity, danceability, loudness, mode_f, speechiness, valence, duration_mins, genre),
          
          sortCol = popularity)

ggsave(filename = "C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/DATA 824/Final Project/figures/plot_table.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)



g <- ggplot(TikTok.data, aes(popularity)) + scale_fill_brewer(palette = "Spectral")

g + geom_histogram(aes(fill=genre), 
                   bins=10 , 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="TikTok tracks' popularity across genres")  

ggsave(filename = "C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/DATA 824/Final Project/figures/histogram.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)


d <- ggplot(TikTok.data, aes(popularity)) + scale_fill_brewer(palette = "Spectral")

d + geom_histogram(aes(fill=mode_f), 
                   bins=10 , 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="TikTok tracks' popularity across different modes")  

ggsave(filename = "C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/DATA 824/Final Project/figures/histogram_mode.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)
###It shows that the minor mode is less frequent among popular tracks than tracks with major mode. 

#  scatterplots  
TikTok.data %>% 
  ggplot(aes(x = danceability, y = popularity)) +
  geom_point(position = "jitter",
             color = "black",
             alpha = 1/10,
             size = 1.5) +
  scale_x_continuous(breaks = seq(0,1,0.2)) +
  scale_y_continuous(breaks = seq(0,100,20)) +
  xlab("danceability") +
  ylab("popularity") +
  ggtitle("TikTok popularity VS danceability - scatterplot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

ggsave(filename = "C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/DATA 824/Final Project/figures/scatterplot1.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)