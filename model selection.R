###Regression

TikTok = read.csv("C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/STAT840/Final project/Proposal/tiktok1.csv")
TikTok.data = read.csv("C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/STAT840/Final project/Proposal/tiktok.csv")
######################
head(TikTok)

###Recode
TikTok.data$mode_f = is.factor(TikTok.data$mode)
TikTok.data$mode_f <- factor(TikTok.data$mode, labels = c("minor", "major"))

attach(TikTok.data)
m1 <- lm(popularity~danceability + instrumentalness + tempo + loudness + genre + energy + valence, data=TikTok.data)
summary(m1)
########################################################################################################
# Exploratory data analysis to examine distribution of explanatory variables, identify outliers, and   #
# identify potential relationships between explanatory variables, and identify nature of relationships #
# between explanatory variables and outcome. Unusual values should be flagged as they may influence    #
# the fit of the model.                                                                                #  
########################################################################################################
for (i in c(8,9, 10,14, 16,17, 21)){
  boxplot(TikTok.data[,i], main = names(TikTok.data)[i])
  stripchart(TikTok.data[,i], vertical = T, method = "jitter", add = TRUE)
}

##########
########################################################################################################
# Screening of Explanatory Variables                                                                   #
# Partial residual plots and stepwise regression help narrow down the candidate explanatory variables. #
# Added variable plots (also known as partial residual plots or adjusted variable plots) provide       #
# evidence of the importance of a covariate given the other covariates already in the model. They also #
# display the nature of the relationship between the covariate and the outcome (i.e., linear,          #
# curvilinear, transformation necessary, etc.) and any problematic data points with respect to the     #
# predictor.

prplot(m1,1) 
prplot(m1,2) 
prplot(m1,3) 
prplot(m1,4)
prplot(m1,5)
prplot(m1,6)
prplot(m1,7)

GGally::ggpairs(c(7,8,9, 10,14, 16,17), dataset = TikTok.data)
ggsave(filename = "C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/DATA 824/Final Project/figures/scatter.plot.matrix.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)

###the scatter plot matrix
pairs(TikTok.data[,c(8,9, 10,14, 16,17)], pch = 19)
###It shows energy and loudness are corrolated
ggsave(filename = "C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/DATA 824/Final Project/figures/scatter.matrix.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)
cor(TikTok.data [,c(8,9, 10,14, 16,17)])

library(corrplot)
L = cor(TikTok.data [,c(8,9, 10,14, 16,17)])
corrplot(L, method = 'number') # colorful number

ggsave(filename = "C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/DATA 824/Final Project/figures/cor.num.matrix.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)
corrplot(L, method = 'color')
##Energy has strong corrolation with loudness and is also corrrlated with valence. 
ggsave(filename = "C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/DATA 824/Final Project/figures/cor.matrix.color.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)
# Automatic variable selection methods can be a useful starting point in eliminating redundant variables. 
# They should only be used as a guide to the screening and removal (or addition) of predictors. 

fa <- regsubsets(popularity~danceability + instrumentalness + tempo + loudness + genre + energy + valence, data=TikTok.data) 
fma <- summary(fa)
fma
names(fma)
fma$rsq # bigger is better

plot(1:8,fma$rsq, pch=16, xlab="Number of Parameters",ylab=expression(R[p]^2))

fma$rss # smaller is better, model 5 (<1% decrease for addition of log(income))
plot(1:8,fma$rss, pch=16, xlab="Number of Parameters",ylab=expression(SSE[p]))

fma$adjr2 # bigger is better, model 5
plot(1:8,fma$adjr2, pch=16, xlab = "Number of Parameters", ylab = expression(R[a,p]^2))

fma$cp # Cp = p is better, model 5
plot(1:8, fma$cp,  pch=16, xlab = "Number of Parameters", ylab = expression(C[p]))
abline(0,1)

fma$bic # smaller is better, model 5
plot(1:8, fma$bic,  pch=16, xlab = "Number of Parameters", ylab = expression(BIC[p]))

# Consensus evidence is to drop energy and valence.

################################################################

# TikTok popularity prediction models
# some guidelines:
#   - we will try different predictor variables (different number of predictors and different predictors)
#   - we won't apply any variable transformation (price will be used, not log(price))
#   - technique cross-validation will be used: 80% data - train dataset & 20% data - test dataset
#   - possible predictors (independant variables): carat, volume, cut, clarity, color
#   - variable try to predict (dependant variable): price
#   - prediction model: linear regression model
#   - for the purpose of demonstration not all dataset variables are used for predictor
#   - we consider bias ~ variance trade off
#   - Root Mean Square Error (RMSE) metrics used to compare models


# first lets find out how many different models we can build
colnames(TikTok.data)
possible.predictors <- colnames(TikTok.data)[c(8,9,10,12, 13, 14, 15, 16,17, 20, 21, 22)] # possible predictors
possible.predictors

# generate all possible combinations of predictors
df.models <- NULL # here we store all possible models
model.count <- 1 # counter for model 

for(nr.predictors in 1:2){ 
  
  predictors <- combn(x = possible.predictors, m = nr.predictors) # generate all possible combinations of predictors
  
  for(combination in 1:ncol(predictors)){ # loop over every combination of predictors
    
    predictors.list <- paste0(predictors[,combination], collapse = "|") # all predictors
    formula <- paste0("popularity~",paste0(predictors[,combination], collapse = "+")) # model formula
    
    df.models <- rbind(df.models, c(model.count, nr.predictors, predictors.list, formula))
    model.count <- model.count + 1 # increase model count
  }
}

colnames(df.models) <- c("id", "nr. predictors", "predictors", "formula") # column names

#convert data in data frame and cread a numeric id column. 
df.models <- df.models %>% 
  as.data.frame() %>% # convert to data frame
  mutate(id = as.numeric(as.character(id))) # convert to numeric

# Generate all possible models (with function)
#TikTok.models <- generate.models(predictors.vars = possible.predictors, outcome.var = "popularity")

# Split data frame train ~ test
set.seed(123)
id.rows <- 1:nrow(TikTok.data) # all rows ids
id.train <- sample(x = id.rows, size = round(0.8 * nrow(TikTok.data)), replace = F) # train rows
id.test <- setdiff(id.rows, id.train) # test rows- removes the ro


# # write back to data frame; add a new col name sample
TikTok.data[id.train,"sample"] <- "train"
TikTok.data[id.test,"sample"] <- "test"
#check the test and train size
TikTok.data%>% count(sample)
# Split sample (with function)
set.seed(123)

#TikTok.data <- split.sample(TikTok.data)
TikTok.data.train <- TikTok.data %>% filter(sample == "train")
TikTok.data.test <- TikTok.data %>% filter(sample == "test")


# Train models (train dataset), predict popularity (test dataset), calculate RMSE (test dataset)

df.models <- df.models %>%# add RMSE column
  mutate(RMSE = NA)

for(id.model in 1:nrow(df.models)){ # loop over each model
  # train model
  formula <- df.models[id.model,"formula"]
  lm.model <- lm(formula = formula, data = TikTok.data.train)
  
  # predict popularit (test dataset)
  TikTok.data.test <- TikTok.data.test %>% 
    mutate(`popularity predicted` = predict(lm.model, .)) 
  
  # calculate RMSE (predicted popularity VS actual popularity)
  SSE <- (TikTok.data.test$`popularity predicted` - TikTok.data.test$popularity)^2 # sum of squared errors
  RMSE <- sqrt(mean(SSE))
  # write RMSE back to table
  df.models[id.model,"RMSE"] <- RMSE
}
#Small RMSE indicates a better prediction
# draw model performance
predictors.levels <- df.models %>% arrange(id) %>% pull(predictors) # levels for predictors factor variable


df.models %>% 
  mutate(`nr. predictors` = as.factor(as.character(`nr. predictors`)),
         predictors = factor(predictors, levels = predictors.levels)) %>% 
  ggplot(aes(x = predictors, y = RMSE, fill = `nr. predictors`)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks = seq(0, 5000, 500)) +
  scale_fill_viridis_d() +
  xlab("Predictors") +
  ylab("RMSE") +
  ggtitle("popularity prediction model performance") +
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))
ggsave(filename = "C:/Users/MB/Desktop/fateme files_JULY 2021/Courses/Fall 2021/DATA 824/Final Project/figures/prediction.model.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)

####################################################################################################




