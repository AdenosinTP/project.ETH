#
#
#  //// Analyze different spiking frequencies of the neurons in one core in Dynap-se ////
#
#
#
# With the .txt data files that are the result of rasterplot.py, this script permets the comparison
# between the frequencies of up to 3 neurons in one Core (core_id= 0,1,2,3).
# Each .txt file should be the data of a full recording of the spikes in fuction of time of each of the 256 neurons present
# in the 4 cores of Dynap-se
#
# Author : Valerio Tettamanti valerite@student.ethz.ch


###########################################################################################


# First, go on session -> set working directory -> choose directory  and select the folder with your .txt files


# activate library for ggplot ; if you don't have it -> install.packages("ggplot2")
library(ggplot2)

# Function for reading the files 
files = list.files( pattern = "*.txt")
n = length(files)

freq1.raw = lapply(files, function(x) {
  file1 <- read.table(x, header = T)
  # here you can choose which neuron you want to use [0-255] and from which core taking it
  neuron_time =  file1$time[file1$core_id==0 & file1$neuron_id==0]
  spikes = length(neuron_time)
  min = min(neuron_time)
  max = max(neuron_time)
  dtime = (max - min)/1000000
  freq_n0 = spikes/(dtime)
  freq_n0
  return(freq_n0)  #the result will be in us

    })

freq2.raw = lapply(files, function(x) {
  file1 <- read.table(x, header = T)
  # here you can choose which neuron you want to use [0-255] and from which core taking it
  neuron_time =  file1$time[file1$core_id==0 & file1$neuron_id==1]
  spikes = length(neuron_time)
  min = min(neuron_time)
  max = max(neuron_time)
  dtime = (max - min)/1000000
  freq_n0 = spikes/(dtime)
  freq_n0
  return(freq_n0)  #the result will be in us
  
})

freq3.raw = lapply(files, function(x) {
  file1 <- read.table(x, header = T)
  # here you can choose which neuron you want to use [0-255] and from which core taking it
  neuron_time =  file1$time[file1$core_id==0 & file1$neuron_id==3]
  spikes = length(neuron_time)
  min = min(neuron_time)
  max = max(neuron_time)
  dtime = (max - min)/1000000
  freq_n0 = spikes/(dtime)
  freq_n0
  return(freq_n0)  #the result will be in us
  
})

# the result is a list of the frequencies for the neuron -> unlisting as we need a vector
freq1 = unlist(freq1.raw)
freq2 = unlist(freq2.raw)
freq3 = unlist(freq3.raw)

# prepare new matrix for ggplot2 
n_data = c(1:8)*32-1                   # number of files you are using, in my case 0:255 by 32 jumps
data_1 = data.frame(n_data, freq1)
data_2 = data.frame(n_data, freq2)
data_3 = data.frame(n_data, freq3)

ggplot() + geom_line(data=data_1, aes(n_data, freq1), colour= 'red') + geom_point(data=data_1, aes(n_data, freq1)) +
  geom_line(data=data_2, aes(n_data, freq2), colour= 'green') + geom_point(data=data_2, aes(n_data, freq2)) +
  geom_line(data=data_3, aes(n_data, freq3), colour= 'blue') + geom_point(data=data_3, aes(n_data, freq3)) +
  labs(x="fValue", y = "Frequency [Hz]") +
  theme(plot.title = element_text(hjust = 0.5)) 




