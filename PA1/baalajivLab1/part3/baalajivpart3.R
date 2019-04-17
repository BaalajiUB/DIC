library(shiny)
library(plyr)
library(plotly)
library(ggmap)
library(dygraphs)
library(tidyverse)

df <- read.csv("2011_us_ag_exports.csv", header = T)

#write.csv(df, '2011_us_ag_exports.csv')
#head(df)
df <- df[, c(1,2,3,4)]

chart5_data <- read.csv(file = 'StateDataforMap_2018-19week8.csv', header = T, stringsAsFactors = F)
#head(chart5_data)
chart5_data <- chart5_data[,-c(2,3)]

#https://stackoverflow.com/questions/42286072/extracting-numeric-values-from-mixed-variable-column-in-r
chart5_data$ACTIVITY.LEVEL <- sapply(strsplit(chart5_data$ACTIVITY.LEVEL, ' '), function(x) as.numeric(x[[2]]))

req_data <- left_join(chart5_data, df, by = c("STATENAME"="state"))

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)

# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

req_data$hover <- with(req_data, paste('Level ',ACTIVITY.LEVEL ))

cdcData <- plot_geo(req_data, locationmode = 'USA-states') %>%
  add_trace(
    z = ~ACTIVITY.LEVEL,text = ~hover,  locations = ~code,
    color = ~ACTIVITY.LEVEL, colors = 'Reds', legendgroup = ~ACTIVITY.LEVEL.LABEL
  ) %>%
  colorbar(title = "ACTIVITY LEVEL") %>%
  layout(
    title = 'CDC heatmap',
    geo = g
  )


#-------------------------------Twitter 1---------------------------------


##ETL
flu1 <- read.csv('flu1.csv', header = T, stringsAsFactors = F)
dim(flu1)
#flu1 <- rt

locations <- flu1$location
#View(table(locations))
#View(unique(locations))

temp1 <- as.vector(locations)
#https://stat545.com/block022_regular-expression.html
temp1 <- temp1[grepl(', [A-Z]{2}$',temp1)]
#View(temp1)

temp2 <- c() #appending to a list
for (i in temp1){
  temp2<-c(temp2,strsplit(i, ',')[[1]][2])
}

temp2
length(temp2)

tbl <- table(temp2)
population <- as.data.frame(tbl, stringsAsFactors = F)
population$temp2 <- trimws(population$temp2) #trimws works on entire column

tmp <- as.vector(locations)

temp3 <- c() #appending to a list
for (i in tmp){
  if (length(strsplit(i, ',')[[1]]) == 2){
    x <- strsplit(i, ',')[[1]][2]
    if ("USA" == trimws(x)){
      temp3<-c(temp3,strsplit(i, ',')[[1]][1])}
  }
}

tbl1 <- table(temp3)
population1 <- as.data.frame(tbl1, stringsAsFactors = F)
population1$temp3 <- trimws(population1$temp3) #trimws works on entire column


library(tidyverse)
library(usmap)

req_data <- merge(x = population, y = statepop, x.by = temp2, y.by = abbr, x.all = TRUE)
req_data <- filter(req_data, temp2 == abbr)
#View(req_data)
req_data <- req_data[, c(2,3,4,5)]
names(req_data)

req_data1 <- merge(x = population1, y = statepop, x.by = temp3, y.by = full, x.all = TRUE)
req_data1 <- filter(req_data1, temp3 == full)
#req_data <- req_data[, c(2,3,4,5)]
req_data1 <- req_data1[, c(2,3,4,5)]
names(req_data1) <- c('Freq_1', 'fips', 'abbr', 'full')

res <- merge(x = req_data,y = req_data1, all = T)
res$Freq[is.na(res$Freq)] <- 0
res$Freq_1[is.na(res$Freq_1)] <- 0
res$frequency <- res$Freq + res$Freq_1
names(res)
res <- res[, c(1,2,3,6)]
#nchar(req_data$temp2)
#nchar(trimws(req_data$temp2))
#View(res)
# heatmap <- plot_usmap(data = res, values = "frequency", lines = "black") +
#   #scale_fill_manual(values = c( "#CC0000", "#D73800", "#E27100", "#EEAA00", "#F9E200", "#E2F800", "#AAEA00", "#71DD00", "#38CF00", "#00C200")) +
#   theme(legend.position = "right") +
#   labs(fill = "Population")
#
# heatmap
##############################################################################################################################
#library(tidyverse)
df <- read.csv("2011_us_ag_exports.csv", header = T)
head(df)
#write.csv(df, '2011_us_ag_exports.csv')
#head(df)
df <- df[, c(2,3,4,5)]
names(df)
#chart5_data <- read.csv(file = 'StateDataforMap_2018-19week8.csv', header = T, stringsAsFactors = F)
#head(chart5_data)
chart5_data <- res
#names(res)
#chart5_data <- chart5_data[,-c(2,3)]

#https://stackoverflow.com/questions/42286072/extracting-numeric-values-from-mixed-variable-column-in-r
#chart5_data$ACTIVITY.LEVEL <- sapply(strsplit(chart5_data$ACTIVITY.LEVEL, ' '), function(x) as.numeric(x[[2]]))

req_data <- left_join(chart5_data, df, by = c("abbr"="code"))

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)

# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
names(req_data)
req_data$hover <- with(req_data, frequency)

twitter1 <- plot_geo(req_data, locationmode = 'USA-states') %>%
  add_trace(
    z = ~frequency,text = ~hover,  locations = ~abbr,
    color = ~frequency, colors = 'Reds'
  ) %>%
  colorbar(title = "ACTIVITY LEVEL") %>%
  layout(
    title = 'Twitter Flu heatmap',
    geo = g
  )


#----------------------------Twitter 2 ----------------------------------

##ETL
flu1 <- read.csv('flu_18k_1.csv', header = T, stringsAsFactors = F)
dim(flu1)
#flu1 <- rt

locations <- flu1$location
#View(table(locations))
#View(unique(locations))

temp1 <- as.vector(locations)
#https://stat545.com/block022_regular-expression.html
temp1 <- temp1[grepl(', [A-Z]{2}$',temp1)]
#View(temp1)

temp2 <- c() #appending to a list
for (i in temp1){
  temp2<-c(temp2,strsplit(i, ',')[[1]][2])
}

temp2
length(temp2)

tbl <- table(temp2)
population <- as.data.frame(tbl, stringsAsFactors = F)
population$temp2 <- trimws(population$temp2) #trimws works on entire column

tmp <- as.vector(locations)

temp3 <- c() #appending to a list
for (i in tmp){
  if (length(strsplit(i, ',')[[1]]) == 2){
    x <- strsplit(i, ',')[[1]][2]
    if ("USA" == trimws(x)){
      temp3<-c(temp3,strsplit(i, ',')[[1]][1])}
  }
}

tbl1 <- table(temp3)
population1 <- as.data.frame(tbl1, stringsAsFactors = F)
population1$temp3 <- trimws(population1$temp3) #trimws works on entire column


library(tidyverse)
library(usmap)

req_data <- merge(x = population, y = statepop, x.by = temp2, y.by = abbr, x.all = TRUE)
req_data <- filter(req_data, temp2 == abbr)
#View(req_data)
req_data <- req_data[, c(2,3,4,5)]
names(req_data)

req_data1 <- merge(x = population1, y = statepop, x.by = temp3, y.by = full, x.all = TRUE)
req_data1 <- filter(req_data1, temp3 == full)
#req_data <- req_data[, c(2,3,4,5)]
req_data1 <- req_data1[, c(2,3,4,5)]
names(req_data1) <- c('Freq_1', 'fips', 'abbr', 'full')

res <- merge(x = req_data,y = req_data1, all = T)
res$Freq[is.na(res$Freq)] <- 0
res$Freq_1[is.na(res$Freq_1)] <- 0
res$frequency <- res$Freq + res$Freq_1
names(res)
res <- res[, c(1,2,3,6)]
#nchar(req_data$temp2)
#nchar(trimws(req_data$temp2))
#View(res)
# heatmap <- plot_usmap(data = res, values = "frequency", lines = "black") +
#   #scale_fill_manual(values = c( "#CC0000", "#D73800", "#E27100", "#EEAA00", "#F9E200", "#E2F800", "#AAEA00", "#71DD00", "#38CF00", "#00C200")) +
#   theme(legend.position = "right") +
#   labs(fill = "Population")
#
# heatmap
##############################################################################################################################
#library(tidyverse)
df <- read.csv("2011_us_ag_exports.csv", header = T)
head(df)
#write.csv(df, '2011_us_ag_exports.csv')
#head(df)
df <- df[, c(2,3,4,5)]
names(df)
#chart5_data <- read.csv(file = 'StateDataforMap_2018-19week8.csv', header = T, stringsAsFactors = F)
#head(chart5_data)
chart5_data <- res
#names(res)
#chart5_data <- chart5_data[,-c(2,3)]

#https://stackoverflow.com/questions/42286072/extracting-numeric-values-from-mixed-variable-column-in-r
#chart5_data$ACTIVITY.LEVEL <- sapply(strsplit(chart5_data$ACTIVITY.LEVEL, ' '), function(x) as.numeric(x[[2]]))

req_data <- left_join(chart5_data, df, by = c("abbr"="code"))

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)

# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
names(req_data)
req_data$hover <- with(req_data, frequency)

twitter2 <- plot_geo(req_data, locationmode = 'USA-states') %>%
  add_trace(
    z = ~frequency,text = ~hover,  locations = ~abbr,
    color = ~frequency, colors = 'Reds'
  ) %>%
  colorbar(title = "ACTIVITY LEVEL") %>%
  layout(
    title = 'Twitter heatmap',
    geo = g
  )


#-----------Done------------------

ui <- fluidPage(

  headerPanel("Heat map comparison"),

  sidebarPanel(
    selectInput("plot", "Maps:", 
                c("CDC vs Twitter Flu" = "op1",
                  "Twitter Flu vs Twitter" = "op2",
                  "CDC vs Twitter" = "op3"))),

  mainPanel(plotlyOutput("plot1"), plotlyOutput("plot2"))

)



server <- function(input, output) {
  output$plot1 <- renderPlotly({
    
    if (input$plot=='op2') {
      twitter1;
    } else {
      cdcData
    }
    
  })
  output$plot2 <- renderPlotly({
    if (input$plot=='op1') {
      twitter1;
    } else {
      twitter2;
    }
  })
}


shinyApp(ui, server)
