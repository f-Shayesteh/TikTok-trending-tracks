#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
dataset <- "https://github.com/romeoben/DSC7-Sprint2-TeamDan/raw/main/data/_TIKTOK_playlist_tracks_data.csv"
TikTok<- read.csv(dataset)

library(shiny)
library(dplyr)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  radioButtons("numberOfPredictors","predictor numbers",
               choices = c("one predictor", "two predictors","three predictors"),
               selected = "two predictors"),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
      if (input$numberOfPredictors == "two predictors") 
        {   
        colnames(TikTok)
        possible.predictors <- colnames(TikTok)[c(9,16, 19, 12)] # possible predictors
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
        id.rows <- 1:nrow(TikTok) # all rows ids
        id.train <- sample(x = id.rows, size = round(0.8 * nrow(TikTok)), replace = F) # train rows
        id.test <- setdiff(id.rows, id.train) # test rows- removes the ro
        
        
        # # write back to data frame; add a new col name sample
        TikTok[id.train,"sample"] <- "train"
        TikTok[id.test,"sample"] <- "test"
        #check the test and train size
        TikTok%>% count(sample)
        # Split sample (with function)
        set.seed(123)
        
        #TikTok.data <- split.sample(TikTok.data)
        TikTok.train <- TikTok%>% filter(sample == "train")
        TikTok.test <- TikTok %>% filter(sample == "test")
        
        
        # Train models (train dataset), predict popularity (test dataset), calculate RMSE (test dataset)
        
        df.models <- df.models %>%# add RMSE column
          mutate(RMSE = NA)
        
        for(id.model in 1:nrow(df.models)){ # loop over each model
          # train model
          formula <- df.models[id.model,"formula"]
          lm.model <- lm(formula = formula, data = TikTok.train)
          
          # predict popularit (test dataset)
          TikTok.test <- TikTok.test %>% 
            mutate(`popularity predicted` = predict(lm.model, .)) 
          
          # calculate RMSE (predicted popularity VS actual popularity)
          SSE <- (TikTok.test$`popularity predicted` - TikTok.test$popularity)^2 # sum of squared errors
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
      }
      
      if (input$numberOfPredictors == "one predictor") 
      {  colnames(TikTok)
        possible.predictors <- colnames(TikTok)[c(9,16, 19, 12)] # possible predictors
        possible.predictors
        
        # generate all possible combinations of predictors
        df.models <- NULL # here we store all possible models
        model.count <- 1 # counter for model 
        
        for(nr.predictors in 1){ 
          
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
        id.rows <- 1:nrow(TikTok) # all rows ids
        id.train <- sample(x = id.rows, size = round(0.8 * nrow(TikTok)), replace = F) # train rows
        id.test <- setdiff(id.rows, id.train) # test rows- removes the ro
        
        
        # # write back to data frame; add a new col name sample
        TikTok[id.train,"sample"] <- "train"
        TikTok[id.test,"sample"] <- "test"
        #check the test and train size
        TikTok%>% count(sample)
        # Split sample (with function)
        set.seed(123)
        
        #TikTok.data <- split.sample(TikTok.data)
        TikTok.train <- TikTok%>% filter(sample == "train")
        TikTok.test <- TikTok %>% filter(sample == "test")
        
        
        # Train models (train dataset), predict popularity (test dataset), calculate RMSE (test dataset)
        
        df.models <- df.models %>%# add RMSE column
          mutate(RMSE = NA)
        
        for(id.model in 1:nrow(df.models)){ # loop over each model
          # train model
          formula <- df.models[id.model,"formula"]
          lm.model <- lm(formula = formula, data = TikTok.train)
          
          # predict popularit (test dataset)
          TikTok.test <- TikTok.test %>% 
            mutate(`popularity predicted` = predict(lm.model, .)) 
          
          # calculate RMSE (predicted popularity VS actual popularity)
          SSE <- (TikTok.test$`popularity predicted` - TikTok.test$popularity)^2 # sum of squared errors
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
      
      }
      if (input$numberOfPredictors == "three predictors") 
      {   
        colnames(TikTok)
        possible.predictors <- colnames(TikTok)[c(9,16, 19, 12)] # possible predictors
        possible.predictors
        
        # generate all possible combinations of predictors
        df.models <- NULL # here we store all possible models
        model.count <- 1 # counter for model 
        
        for(nr.predictors in 1:3){ 
          
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
        id.rows <- 1:nrow(TikTok) # all rows ids
        id.train <- sample(x = id.rows, size = round(0.8 * nrow(TikTok)), replace = F) # train rows
        id.test <- setdiff(id.rows, id.train) # test rows- removes the ro
        
        
        # # write back to data frame; add a new col name sample
        TikTok[id.train,"sample"] <- "train"
        TikTok[id.test,"sample"] <- "test"
        #check the test and train size
        TikTok%>% count(sample)
        # Split sample (with function)
        set.seed(123)
        
        #TikTok.data <- split.sample(TikTok.data)
        TikTok.train <- TikTok%>% filter(sample == "train")
        TikTok.test <- TikTok %>% filter(sample == "test")
        
        
        # Train models (train dataset), predict popularity (test dataset), calculate RMSE (test dataset)
        
        df.models <- df.models %>%# add RMSE column
          mutate(RMSE = NA)
        
        for(id.model in 1:nrow(df.models)){ # loop over each model
          # train model
          formula <- df.models[id.model,"formula"]
          lm.model <- lm(formula = formula, data = TikTok.train)
          
          # predict popularit (test dataset)
          TikTok.test <- TikTok.test %>% 
            mutate(`popularity predicted` = predict(lm.model, .)) 
          
          # calculate RMSE (predicted popularity VS actual popularity)
          SSE <- (TikTok.test$`popularity predicted` - TikTok.test$popularity)^2 # sum of squared errors
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
      }
      
      
      
      })
     
    output$value <- renderPrint({ input$checkGroup })
}
       
# Run the application 
shinyApp(ui = ui, server = server)
