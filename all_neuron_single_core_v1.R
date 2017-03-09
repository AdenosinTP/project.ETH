#  
#  
#           /// Visualisation of frequencies changes relative to current input variation 
#                   in Dynap-se Neuromorphic chip's 256 neurons (of a single core)    ///
# 
# 
# This script can analyze the data extracted from the .aedat files recorded with
# cAER, by using the python script found in the repository 
# (https://github.com/AdenosinTP/project_ETH), and show the frequency of each 
# neuron for an increasing current (in this case DC coarseValue was costant and
# changes were in fineValue with the rest of the parameters unchanged from the initial ones)
# 
# 
# Author : Valerio Tettamanti valerite@student.ethz.ch


###########################################################################################


# First, go on session -> set working directory; choose directory 
# select the folder with your .txt files


# activate library for ggplot ; if you don't have it -> install.packages("ggplot2")
library(ggplot2)
library(plyr)
library(reshape2)

# Reading all the files *.txt
files = list.files( pattern = "*.txt")
n = length(files)

# extract the time of each spike (for each one of the 256 neurons) and calculate
# the frequency for each current change

frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
   
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)

  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==1 & file$core_id==0 & file$neuron_id==i]
    spikes = length(neuron_time)
    min = min(neuron_time)
    max = max(neuron_time)
    dtime = (max - min)/1000000
    single_freq = spikes/(dtime)
      if (single_freq == 0) {
        freq[i+1] = 0
      } else {
        freq[i+1] = single_freq   # exits the  frequency of neuron_id = i for the i neuron
      }
  }
  
  return(freq)
  })

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:5), sep="")


df = melt(frequencies)
names(df)

# plottig the frequencies changes for 256 neurons
ggplot(df, aes(Var2, value, group=factor(Var1))) + geom_line(aes(color=factor(Var1))) + 
  theme(legend.position="none") +
  labs(x="fValue", y = "Frequency [Hz]") +
  labs(title = "Core 0") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("all_neurons_freq_core_0.png", plot = last_plot())







