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

chart1 <- ggplot() + 
  geom_bar(data = bar_data,mapping = aes(fill=variable, y=value, x=factor(Week)), position="stack", stat="identity", color = "black") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
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
        axis.line.x.top = element_line(size=0.5),
        axis.line.y = element_line(size = 1),
        legend.title = element_blank() ,
        legend.key.width = unit(1,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.text = element_text(face = "bold",margin = margin(1.5,1.5,1.5,1.5,"mm")), #to add space between legend symbol and name
        legend.key = element_rect(fill = "white"), #to make legend symbol background white
        panel.background = element_rect(fill = "white", colour = NA))   #to make the chart background white

chart1

rm(bar, line)


#chart2
chart2_data <- read.csv('chart2_data.csv',header = T)
