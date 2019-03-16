library(ggplot2)
setwd('/Users/tiffanyong/Documents/GitHub/gfw3/s')

files = list.files(path = paste("../data/monthly_effort_CONTROL_4YEAR/", sep=''), pattern = NULL, all.files = FALSE,
                   full.names = FALSE, recursive = FALSE,
                   ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

files #check filenames here

len = length(files) #how many Rdatafiles are in the folder 

Rdata = list() 
for(i in 5:len) {
  eez = files[[i]]
  file_name = paste("../data/monthly_effort_CONTROL_4YEAR/", eez,  sep = "")
  load(file_name)
  Rdata[[i]] =  finaldf
}

#########PLOTTING
num=2
eez = files[num]
eez

#paste what eez is without .Rdata
geoname = 'Clipperton Exclusive Economic Zone'

file_name = paste("../data/monthly_effort_CONTROL_4YEAR/", eez,  sep = "")
load(file_name)
file_name

title = paste('../Figures/CONTROL_EEZs_4YEAR/', geoname, '.png', sep = '')
title
png(title, units="in", width=13, height=7, res=300)

#quartz()
ggplot(Rdata[[num]], aes(months_o_year, finaldf$fishing_effort,group=1)) + 
  
  geom_point(size=0.1,stroke = 0, shape = 16) + #makes the points disappear
  geom_line(size=0.2) +
  
  labs(x="Month", y="Monthly Fishing Effort", 
       title = geoname, 
       subtitle = 'January 2014 - January 2018') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b %y") +
  geom_vline(aes(xintercept = months_o_year[[13]], color='dodgerblue'), color="dodgerblue", size=0.4) +
  geom_vline(aes(xintercept = months_o_year[[25]], color='dodgerblue'), color="dodgerblue", size=0.4) +
  geom_vline(aes(xintercept = months_o_year[[37]], color='dodgerblue'), color="dodgerblue", size=0.4)

dev.off()


#####TESTING WITH TIME SERIES######

#original plot
df = finaldf
ma.5 <- filter(df,filter=rep(1/5,5))

quartz()
ggplot(ma.5, aes(months_o_year, ma.5[,2])) + 
  
  geom_point(size=0.1,stroke = 0, shape = 16) + #makes the points disappear
  geom_line(size=0.2) +
  
  labs(x="Month", y="Monthly Fishing Effort", 
       title = geoname, 
       subtitle = 'January 2014 - January 2018') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b %y")


library(ggfortify)

quartz() 
autoplot(ts) + theme_minimal()

df = finaldf$fishing_effort #Clipperton EEZ
ts = ts(df)

ma.3 <- filter(ts,filter=rep(1/3,3))
ma.4 <- filter(ts,filter=rep(1/4,4))
ma.5 <- filter(ts,filter=rep(1/5,5))
ma.10 <- filter(ts,filter=rep(1/10,10))

quartz()
plot(ts)
quartz()
plot(ma.10) 