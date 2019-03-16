library(ggplot2)
setwd('/Users/tiffanyong/Documents/GitHub/gfw3/s')

#GETS RDATA OF THE TOP FISHERS IN A CERTAIN MPA

#TODO: fill in mpa name
mpa_name = "Papahānaumokuākea Marine National Monument"
path = paste("../data/top_fishers_effort/", mpa_name, sep = "") 

files = list.files(path = path, pattern = NULL, all.files = FALSE,
                   full.names = FALSE, recursive = FALSE,
                   ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
files #check file names here
len = length(files) #how many Rdatafiles are in the folder 

Rdata = list() #list of 

for(i in 1:len) {
  file = files[[i]]
  file_name = paste(path, "/", file,  sep = "")
  load(file_name)
  Rdata[[i]] =  thisdf3 #this loads in aggregate data for 24 months
}

#this gets the data of teh MPA
file_name = paste("../data/monthly_effort_scatterplot/", mpa_name, ".Rdata", sep = '')
load(file_name)
Rdata[[len+1]] = finaldf
Rdata[[len+1]]$date = c(-12:11)
colnames(Rdata[[len+1]]) = c("month", "sum") 
         
###PLOTTING####

specific_title = "2lines"
title = paste('../Figures/MPA_vs_top_fishers/', mpa_name, "/", specific_title, '.png', sep = '')
png(title, units="in", width=13, height=7, res=300)
thick = 0.4

# quartz()
ggplot(Rdata[[1]], aes(x=c(-12:11), y=sum, group=1)) + 
  geom_line(aes(color="All boats"), size = thick) +

  #geom_line(data = Rdata[[4]], aes(color="top 25%"), size = thick) +
  
  geom_line(data = Rdata[[2]], aes(color="MPA"), size = thick+0.1) +
  
  scale_colour_manual(values=c('All boats'="blue", 'MPA' ="black"))+
  labs(color="Legend") +
  
  labs(x="Month", y="Monthly Fishing Effort", 
       title = paste("Monthly Fishing Effort: ", mpa_name, sep = "") , 
       subtitle = "Before and After MPA Creation"
  ) +
  theme_minimal() +
  geom_vline(aes(xintercept = 0, color='black'), color="black", size=0.5)

dev.off()
