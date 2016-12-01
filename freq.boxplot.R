

# go on session -> set working directory -> choose directory 
# activate library for ggplot and plyr; if you don't have it -> install.packages("ggplot2")


library(ggplot2)
library(plyr)

# Function for reading the files 
files = list.files( pattern = "*.txt")
n = length(files)

freq.raw = lapply(files, function(x) {
  file = read.table(x, header = T)
  
  freq_all= rep(NA, 255)          # create an empty array long 255 full of zeros
  
  for (i in 0:255) {             # loop for the neurons (here total of 256 neurons)
    time_n = file$time[file$core_id==0 & file$neuron_id==i]  # select the time for different neuron id
    spikes = length(time_n)
    min = min(time_n)
    max = max(time_n)
    dtime.s = (max - min)/1000000
    freq = spikes/(dtime.s)
    freq_all[i] = freq          # exits the  frequency of neuron_id = i for the i neuron
  }
  
  
  y0 = min(freq_all)
  y25 = quantile(freq_all, 0.25)
  y50 = median(freq_all)
  y75 = quantile(freq_all, 0.75)
  y100 = max(freq_all)
  
  mean_freq = mean(freq_all)
  
  mean_freq
  
  return(data.frame(mean_freq, y0, y25, y50, y75, y100))

  })

df <- ldply(freq.raw, data.frame)

c=1:8

df = cbind(df, data_n=c(1,8)*32-1) # number of files you are using, in my case 8 files; 0:255 by 32


ggplot() + geom_boxplot(data=df, aes(data_n, mean_freq, ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100), stat='identity') +
  labs(title = "Mean Frequencies of the 256 Neurons for different fValues with cV of DC=0") +
    labs(x="fValue", y = "Mean frequency [Hz]")



