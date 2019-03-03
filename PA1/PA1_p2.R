#https://www.cdc.gov/flu/weekly/
library(ggplot2)
setwd("~/git_workspace/DIC/PA1")
influenza_national_summary <- read.csv("InfluenziaNationalSurvey.csv",header = T)
#names(influenza_national_summary) <- c("Week","Total_A","Total_B","Perc_Positive_A","Perc_Positive_B","Total_#_Tested", "Perc_Positive")
names(influenza_national_summary) <- c("Week","A","B","% Positive Flu A","% Positive Flu B","Total_#_Tested", "Percentage Positive")
names(influenza_national_summary)
#head(influenza_national_summary$Week)
#head(influenza_national_summary$Total_A)
#http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization#data-1
#https://rpubs.com/MarkusLoew/226759

##Each group consists of only one observation. Do you need to adjust the group aesthetic?
#Add group=1 in aes
##Error: Discrete value supplied to continuous scale
#Renamed column '%_Positive_A' to 'Perc_Positive_A'

######Chart1#######
###Method 1####
# chart1<-ggplot(data=influenza_national_summary) +
#   geom_bar(aes(x=factor(Week), y=Total_A),stat="identity",fill = "yellow",color = "black") +
#   geom_bar(aes(x=factor(Week), y=Total_B),stat="identity",fill = "green",color = "black") +
#   geom_line(aes(x=factor(Week), y=Perc_Positive*400,group=1),color="black") +
#   geom_line(aes(x=factor(Week), y=Perc_Positive_A*400,group=1),linetype = "dotted",color="orange") +
#   geom_line(aes(x=factor(Week), y=Perc_Positive_B*400,group=1),linetype = "dotted",color="green") +
#   scale_y_continuous(sec.axis = sec_axis(~.*(1/400), name = "Percent positive")) +
#   theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
#   theme(legend.position = c(0, 1), legend.justification = c(0, 0),legend.direction = "vertical")+
#   labs(y = "Number of positive specimens",
#        x = "Week",
#        color = "Parameter")
# chart1


###Method 2####
library(reshape2)
melt_inf <- melt(influenza_national_summary,id=c('Week'))
#https://stackoverflow.com/questions/1686569/filter-data-frame-rows-by-a-logical-condition
bar_data <- melt_inf[melt_inf$variable %in% c("A", "B"), ]
# bar_data
# str(bar_data)
# bar <- ggplot() + 
#   geom_bar(data = bar_data,mapping = aes(fill=variable, y=value, x=factor(Week)), position="stack", stat="identity") +
#   theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
#   scale_fill_manual(values = c("yellow","green")) + 
#   ylim(c(0,14000))
# 
# bar

line_data <- melt_inf[melt_inf$variable %in% c("% Positive Flu A", "% Positive Flu B","Percentage Positive"), ]
# line_data
# str(line_data)
# 
# line_data
line_data$variable <- relevel(line_data$variable,"Percentage Positive")
# #https://stackoverflow.com/questions/43879968/geom-line-groups-and-scale-color-manual-produce-unexpected-error
# #http://www.cookbook-r.com/Manipulating_data/Changing_the_order_of_levels_of_a_factor/
# line <- ggplot() +
#   geom_line(data = line_data,mapping = aes(color = variable, y=value*400, x=factor(Week), group = variable),linetype = "dashed") +
#   theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
#   scale_colour_manual(values = c("black","orange", "green")) +
#   ylim(c(0,14000))
# 
# line

#https://stackoverflow.com/questions/32090073/geom-bar-geom-line-with-different-y-axis-scale
#http://www.sthda.com/english/wiki/ggplot2-line-types-how-to-change-line-types-of-a-graph-in-r-software
#https://html-color-codes.info/colors-from-image/#
#https://stackoverflow.com/questions/46039176/ggplot2-adding-secondary-y-axis-with-different-breaks-and-labels
#https://stackoverflow.com/questions/14771546/remove-legend-title-in-ggplot
#http://www.sthda.com/english/wiki/ggplot2-themes-and-background-colors-the-3-elements
#https://ggplot2.tidyverse.org/reference/element.html
#library(lemon)
chart1 <- ggplot() + 
  geom_bar(data = bar_data,mapping = aes(fill=variable, y=value, x=factor(Week)), position="stack", stat="identity", color = "black") +
  theme(axis.text.x = element_text(angle=65)) +
  scale_fill_manual(values = c("#FFFF33","#008033")) +
  #ylim(c(0,14000)) +
  geom_line(data = line_data,mapping = aes(color = variable, y=value*400, x=factor(Week), group = variable,linetype = variable, size = variable)) +
  scale_y_continuous(sec.axis = sec_axis(~.*(1/400), name = "Percent positive", breaks = seq(0,35,5)),breaks = seq(0,14000, by = 2000),limits = c(0,14000)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotted")) +
  #theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  scale_colour_manual(values = c("black","#FFAA33", "#66D533")) +
  scale_size_manual(values = c(0.9,0.9,0.9)) +
  #ylim(0,14000) +
  xlab("Week") + 
  ylab(c("Number of positive specimens","Percent Positive")) +
  theme(axis.title = element_text(face = "bold"),
        axis.title.y.right = element_text(angle = 90, hjust = 0.5,face = "bold"), 
        axis.text = element_text(face = "bold"),
        axis.line.x.top = element_line(size = 1),
        axis.line.y = element_line(size = 1),
        axis.ticks.x = element_blank(),
        legend.title = element_blank() ,
        legend.key.width = unit(1,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.text = element_text(face = "bold",margin = margin(1.5,1.5,1.5,1.5,"mm")), #to add space between legend symbol and name
        legend.key = element_rect(fill = "white"), #to make legend symbol background white
        panel.background = element_rect(fill = "white", colour = NA))   #to make the chart background white

chart1
rm(bar_data, line_data, melt_inf, influenza_national_summary)

#chart2
chart2_data <- read.csv('chart2_data.csv',header = T)
names(chart2_data) <- c('Week', 'H3N2v','A (H1N1)pdm09','A (H3N2)','A (unable to subtype)','A (subtyping not performed)','B (lineage not performed)','B (Victoria Lineage)','B (Yamagata Lineage)','Total Tested')
#names(chart2_data)
melt_data <- melt(chart2_data, id = c('Week'))
#print(unique(melt_data$variable))
melt_data$variable <- factor(melt_data$variable, levels = c('A (subtyping not performed)','A (H1N1)pdm09','A (H3N2)','H3N2v','B (lineage not performed)','B (Victoria Lineage)','B (Yamagata Lineage)','A (unable to subtype)','Total Tested'))
melt_data <- melt_data[!(melt_data$variable %in% c('A (unable to subtype)','Total Tested')),]
#print(unique(melt_data$variable))
chart2 <- ggplot(data = melt_data) +
          geom_bar(mapping = aes(x = factor(Week),y = value, fill = variable), stat = "identity", color = "black") +
          theme(axis.text.x = element_text(angle=65)) +
          xlab('Week') +
          ylab('Number of positive specimens') +
          scale_fill_manual(values = c('#FFFF33','#FFAA00','#FF2B00','#992BFF','#005533','#99FF00','#66D533')) +
          scale_y_continuous(limits = c(0,3000)) +
          theme(axis.title = element_text(face = "bold"),
            axis.text = element_text(face = "bold"),
            axis.line.y = element_line(size=0.5),
            axis.line.x.top = element_line(size = 0.5),
            axis.ticks.x = element_blank(),
            legend.title = element_blank() ,
            legend.text = element_text(face = "bold",margin = margin(1.5,1.5,1.5,1.5,"mm")), #to add space between legend symbol and name
            legend.key = element_rect(fill = "white"), #to make legend symbol background white
            panel.background = element_rect(fill = "white", colour = NA))   #to make the chart background white

  
chart2
rm(chart2_data, melt_data)

##Chart3
library(stringr)
chart3_data <- read.csv(file = 'chart3_data.csv', header = T)
names(chart3_data)
chart3_data = chart3_data[, !(names(chart3_data) %in% c('All.Deaths', 'Pneumonia.Deaths','Influenza.Deaths'))]
names(chart3_data) <- c('Year','Week','P&I','Expected','Threshold')

melt_data <- melt(chart3_data, id = c('Year','Week'))
#str(melt_data)
dim(melt_data)
melt_data <- melt_data[!(melt_data$Year<2014 | melt_data$Year>2018),] #filtering data below 2014 and above 2018
melt_data <- melt_data[!(melt_data$Year==2014 & melt_data$Week<40),] #filering data earlier to week 40 2014
#melt_data <- melt_data[melt_data$Week%%10==0,]
#melt_data
dim(melt_data)
#melt_data$MMWR_Week <- factor(paste(melt_data$Year,melt_data$Week,sep='-'))
melt_data$MMWR_Week <- factor(as.Date(paste(melt_data$Year, melt_data$Week, 01, sep="-"), "%Y-%U-%u"))
unique(melt_data$MMWR_Week)
#paste(melt_data$Year,melt_data$Week,'01',sep='-')
melt_data <- melt_data[,-c(1,2)]
dim(melt_data)
#names(melt_data)

#https://www.rdocumentation.org/packages/stringr/versions/1.4.0/topics/str_pad
#https://ggplot2.tidyverse.org/reference/scale_date.html
chart3 <- ggplot() +
          geom_line(data = melt_data, mapping = aes(x = as.Date(MMWR_Week), y = value, group = variable, color = variable, linetype = variable)) +
          #geom_smooth(data = melt_data[melt_data$variable=='P&I',], mapping = aes(x=MMWR_Week, y = value, group = variable, color = variable), method = 'lm', formula = y ~ poly(x,40), level = 0) +
          scale_color_manual(values = c('red', 'black', 'black')) +
          scale_linetype_manual(values = c('solid','dashed','solid')) +
          scale_x_date(date_labels = "%Y %U", date_breaks = "10 week") +
          scale_y_continuous(limits = c(4,12)) +
          xlab('MMWR Week') +
          ylab('% All Deaths Due to P & I') +
          theme(axis.title = element_text(face = "bold"),
                axis.text = element_text(face = "bold"),
                axis.text.x = element_text(angle=90),
                axis.line.y = element_line(size=0.5),
                axis.line.x = element_line(size=0.5),
                axis.line.x.top = element_line(size = 0.5),
                #axis.ticks.x = element_blank(),
                axis.ticks.length = unit(1,'mm'),
                legend.title = element_blank() ,
                legend.text = element_text(face = "bold",margin = margin(1.5,1.5,1.5,1.5,"mm")), #to add space between legend symbol and name
                legend.key = element_rect(fill = "white"), #to make legend symbol background white
                panel.background = element_rect(fill = "white", colour = NA))   #to make the chart background white


chart3
rm(chart3_data, melt_data)

##chart4
chart4_data <- read.csv(file = 'chart4_data.csv', header = T, skip = 1) #skip is used to skip n lines from beginning
#names(chart4_data)
#head(chart4_data)
chart4_data <- chart4_data[,-3]
melt_data <- melt(chart4_data, id = c('SEASON','WEEK.NUMBER'))
melt_data$WEEK.NUMBER <- as.Date(paste(melt_data$WEEK.NUMBER, 01, sep="-"), "%Y-%U-%u")
#length(unique(melt_data$WEEK.NUMBER))
#names(melt_data)
chart4 <- ggplot() +
          geom_bar(data = melt_data, mapping = aes(x = WEEK.NUMBER, y = value, fill = variable), stat = "identity", color = "black") +
          #facet_wrap(~ SEASON, ncol = 2) +
          scale_x_date(date_labels = "%Y %U", date_breaks = "6 week") +
          scale_y_continuous(limits = c(0,30), breaks = seq(0,30,5)) +
          scale_fill_manual(values = c('#008000','#00AAFF')) +
          xlab('Week of Death') +
          ylab('Number of deaths') +
          theme(axis.title = element_text(face = "bold"),
                axis.text = element_text(face = "bold"),
                axis.text.x = element_text(angle=90),
                axis.line.y = element_line(size=0.5),
                #axis.line.x = element_line(size=0.5),
                axis.line.x = element_line(size = 0.5),
                #axis.ticks.x = element_blank(),
                axis.ticks.length = unit(1,'mm'),
                legend.title = element_blank() ,
                legend.text = element_text(face = "bold",margin = margin(1.5,1.5,1.5,1.5,"mm")), #to add space between legend symbol and name
                legend.key = element_rect(fill = "white"), #to make legend symbol background white
                legend.position = 'bottom',
                panel.background = element_rect(fill = "white", colour = NA),#to make the chart background white
                legend.box.background = element_rect(color = 'black', size = 2))


chart4
rm(chart4_data, melt_data)

##chart5
