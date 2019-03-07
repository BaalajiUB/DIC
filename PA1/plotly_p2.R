rm(list = ls())
setwd("~/git_workspace/DIC/PA1")
library(plotly)

marker_style <- list(line = list(width = 2, color = 'rgb(0, 0, 0)'));

############################################################################################################################################
                                                          ##Chart1##

chart1_data <- read.csv("InfluenziaNationalSurvey.csv",header = T)
#names(influenza_national_summary) <- c("Week","A","B","% Positive Flu A","% Positive Flu B","Total_#_Tested", "Percentage Positive")
names(chart1_data)
chart1_data$Week <- factor(chart1_data$Week)

chart1 <- plot_ly(chart1_data) %>%
  add_trace(x = ~Week, y = ~Percent.Positive.B, type = 'scatter', mode = 'lines', name = '% Positive Flu B', yaxis = 'y2', color = I("#66D533"), line = list(dash = "dot")) %>%
  add_trace(x = ~Week, y = ~Percent.Positive.A, type = 'scatter', mode = 'lines', name = '% Positive Flu A', yaxis = 'y2', color = I("#FFAA33"), line = list(dash = "dash")) %>%
  add_trace(x = ~Week, y = ~X..Positive, type = 'scatter', mode = 'lines', name = 'Percent Positive', yaxis = 'y2', color = I("black")) %>%
  add_trace(x = ~Week, y = ~Total.B, type = 'bar', name = 'B', color = I("#008033"), marker = marker_style) %>%
  add_trace(x = ~Week, y = ~Total.A, type = 'bar', name = 'A', color = I("#FFFF33"), marker = marker_style) %>%
  layout(title = 'Influenza Positive Tests Reported to CDC by U.S Clinical Laboratories,\n National Summary, 2018-2019 Season',
         xaxis = list(tickangle = 315, autotick = F, dtick = 2, linecolor = toRGB('black')),
         yaxis = list(side = 'left', title = 'Number of Positive Specimens', showgrid = FALSE, zeroline = FALSE, range = c(0,14000), exponentformat = "none", linecolor = toRGB('black'), linewidth = 2),
         barmode = 'stack',
         yaxis2 = list(side = 'right', overlaying = "y", title = 'Percent Positive', showgrid = FALSE, zeroline = FALSE, range = c(0,35), linecolor = toRGB('black'), linewidth = 2))

chart1
rm(chart1_data)
#https://plot.ly/r/reference/#layout-yaxis-exponentformat
#https://stackoverflow.com/questions/37843096/plotly-linetype-poperty-setting-in-r
############################################################################################################################################
                                                          ##Chart2##
chart2_data <- read.csv('chart2_data.csv',header = T)
#names(chart2_data) <- c('Week', 'H3N2v','A (H1N1)pdm09','A (H3N2)','A (unable to subtype)','A (subtyping not performed)','B (lineage not performed)','B (Victoria Lineage)','B (Yamagata Lineage)','Total Tested')
names(chart2_data)
chart2_data$Week <- factor(chart2_data$Week)

chart2 <- plot_ly(chart2_data, x = ~Week, y = ~BVIC, type = 'bar', name = 'B (Yamagata Lineage)', color = I("#66D533"), marker = marker_style) %>%
  add_trace(y = ~BYAM, name = 'B (Victoria Lineage)', color = I("#99FF00"), marker = marker_style) %>%
  add_trace(y = ~B, name = 'B (lineage not performed)', color = I("#005533"), marker = marker_style) %>%
  add_trace(y = ~A.H3N2v., name = 'H3N2v', color = I("#992BFF"), marker = marker_style) %>%
  add_trace(y = ~A.H3., name = 'A (H3N2)', color = I("#FF2B00"), marker = marker_style) %>%
  add_trace(y = ~A..H1N1.pdm09, name = 'A (H1N1)pdm09', color = I("#FFAA00"), marker = marker_style) %>%
  add_trace(y = ~A.Subtyping.not.performed., name = 'A (subtyping not performed)', color = I("#FFFF33"), marker = marker_style) %>%
  layout(title = "\nInfluenza Positive Tests Reported to CDC by U.S Public Health Laboratories,\n National Summary, 2018-2019 Season",
         xaxis = list(tickangle = 315, autotick = F, dtick = 2, linecolor = toRGB('black')),
         yaxis = list(title = 'Number of positive specimens', range = c(0,3000), showgrid = F, linecolor = toRGB('black')), 
         barmode = 'stack')

chart2
rm(chart2_data)
############################################################################################################################################
                                                          ##Chart3##
chart3_data <- read.csv(file = 'chart3_data.csv', header = T)
names(chart3_data)

chart3_data$MMWR_Week <- factor(as.Date(paste(chart3_data$Year, chart3_data$Week, 01, sep="-"), "%Y-%U-%u"))

chart3 <- plot_ly(chart3_data) %>%
  add_trace(x = ~MMWR_Week, y = ~Percent.of.Deaths.Due.to.Pneumonia.and.Influenza, type = 'scatter', mode = 'lines', name = 'P & I', color = I("red")) %>%
  add_trace(x = ~MMWR_Week, y = ~Threshold, type = 'scatter', mode = 'lines', name = 'Epidemic Threshold', color = I("black"), line = list(dash = "solid")) %>%
  add_trace(x = ~MMWR_Week, y = ~Expected, type = 'scatter', mode = 'lines', name = 'Seasonal Baseline', color = I("black"), line = list(dash = "dash")) %>%
  layout(title = "Pneumonia and Influenza Mortality from \nthe National Center for Health Statistics Mortality Surveillance System",
         xaxis = list(title = 'MMWR Week',tickangle = 315, linecolor = toRGB('black'), showgrid = F, tickformat = "%U-%Y", type = "date"),
         yaxis = list(title = '% of All Deaths Due to P&I', range = c(4,12), showgrid = F, linecolor = toRGB('black')))

chart3
rm(chart3_data)
## TO DO ##
#1) Display alternate weeks
#2) Display Year inside the map
#3) Add title last line?
############################################################################################################################################
                                                          ##Chart4##
chart4_data <- read.csv(file = 'chart4_data.csv', header = T, skip = 1) #skip is used to skip n lines from beginning
names(chart4_data)

chart4 <- plot_ly(chart4_data, x = ~WEEK.NUMBER, y = ~CURRENT.WEEK.DEATHS, type = 'bar', name = 'Deaths Reported Current Week', color = I("#00AAFF"), marker = marker_style) %>%
  add_trace(y = ~PREVIOUS.WEEK.DEATHS, name = 'Deaths Reported Previous Week', color = I("#008000"), marker = marker_style) %>%
  layout(title = "Number of Influenzia-Associated Pediatric Deaths\nby week of Death: 2015-16 season to present",
         xaxis = list(title = 'Week of Death',tickangle = 270, autotick = F, dtick = 6, linecolor = toRGB('black')),
         yaxis = list(title = 'Number of deaths', range = c(0,30), showgrid = F, linecolor = toRGB('black')), 
         barmode = 'stack',
         legend = list(orientation = 'h', bordercolor = "black", borderwidth = 2))
chart4
rm(chart4_data)
## TO DO ##
#1) No. of deaths above each SEASON
############################################################################################################################################
                                                          ##Chart5##
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
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

chart5 <- plot_geo(req_data, locationmode = 'USA-states') %>%
  add_trace(
    z = ~ACTIVITY.LEVEL,text = ~hover,  locations = ~code,
    color = ~ACTIVITY.LEVEL, colors = 'Reds'
  ) %>%
  colorbar(title = "ACTIVITY LEVEL") %>%
  layout(
    title = 'heatmap',
    geo = g
  )

chart5
rm( req_data, chart5_data, df, l, g, marker_style)
#https://stackoverflow.com/questions/34465947/plotly-maps-not-rendering-in-r
#Use the "Open in a new window option" (last option under Viewer, which opens the chart in browser) to view the interactive map.
## TO DO ##
#1)Grouped legends
#2)Discrete legends 
############################################################################################################################################
