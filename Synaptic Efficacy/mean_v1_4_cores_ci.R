###########################################################################################

# go on session -> set working directory -> choose directory 
# activate library for ggplot and plyr; if you don't have it -> install.packages("ggplot2")


library(ggplot2)
library(plyr)



start.time <- Sys.time()

# Function for reading the files 
files = list.files( pattern = "*.txt")
n = length(files)




# /////////////////////////// CORE 0  /////////////////////////////////

frequencies = lapply(files, function(x) {
  file = read.table(x, header = T)
  
  freq= rep(NA, 256)          # create an empty array long 255 full of zeros
  
  for (i in 0:255) {             # loop for the neurons (here total of 256 neurons)
    time_n = file$time[file$chip_id==1 & file$core_id==0 & file$neuron_id==i]  # select the time for different neuron id
    spikes = length(time_n)
    if (spikes==1){
      dtime.s = (max(file$time[file$chip_id==1 & file$core_id==0]) - min(file$time[file$chip_id==1 & file$core_id==0]))/1000000
      single_freq = spikes/(dtime.s)
      
      if (single_freq == 0) {
        freq[i+1] = 0
      } else {
        freq[i+1] = single_freq # exits the  frequency of neuron_id = i for the i neuron
      } 
      
    } else {
      
      dtime.s = (max(file$time[file$chip_id==1 & file$core_id==0]) - min(file$time[file$chip_id==1 & file$core_id==0]))/1000000
      single_freq = spikes/(dtime.s)
      if (single_freq == 0) {
        freq[i+1] = 0
      } else {
        freq[i+1] = single_freq # exits the  frequency of neuron_id = i for the i neuron
      } 
    }
    
    
    # exits the  frequency of neuron_id = i for the i neuron
  }
  
  mean_freq = mean(freq, na.rm = TRUE)
  error <- qt(0.975,df=length(freq)-1)*sd(freq)/sqrt(length(freq))
  upr = mean(freq) + error
  lwr = mean(freq) - error
  
  
  
  return(data.frame(mean_freq, error, upr, lwr))
  
})

df <- ldply(frequencies, data.frame)

c=1:8

df = cbind(df, data_n=c(50,75,100,125,150)) # number of files you are using, in my case 8 files; 0:255 by 32


ggplot() + 
  # geom_ribbon(data=df, aes(data_n, ymin=lwr, ymax=upr), alpha=0.3, inherit.aes = T) +
  geom_line(data=df, aes(data_n, mean_freq), colour="#D55E00") +
  geom_errorbar(data=df, aes(data_n, ymin=lwr, ymax=upr), colour="black", width=5) +
  geom_point(data=df, aes(data_n, mean_freq), shape=21, size=2, fill="white") +
  labs(x="Stimulus freq [Hz]", y = "Spiking freq [Hz]") +
  labs(title = "Core 0") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("c0_synaptic_eff_mean.png", plot = last_plot(), width=7, height=7)






# /////////////////////////// CORE 1  /////////////////////////////////

frequencies = lapply(files, function(x) {
  file = read.table(x, header = T)
  
  freq= rep(NA, 256)          # create an empty array long 255 full of zeros
  
  for (i in 0:255) {             # loop for the neurons (here total of 256 neurons)
    time_n = file$time[file$chip_id==1 & file$core_id==1 & file$neuron_id==i]  # select the time for different neuron id
    spikes = length(time_n)
    if (spikes==1){
      dtime.s = (max(file$time[file$chip_id==1 & file$core_id==1]) - min(file$time[file$chip_id==1 & file$core_id==1]))/1000000
      single_freq = spikes/(dtime.s)
      
      if (single_freq == 0) {
        freq[i+1] = 0
      } else {
        freq[i+1] = single_freq # exits the  frequency of neuron_id = i for the i neuron
      } 
      
    } else {
      
      dtime.s = (max(file$time[file$chip_id==1 & file$core_id==1]) - min(file$time[file$chip_id==1 & file$core_id==1]))/1000000
      single_freq = spikes/(dtime.s)
      if (single_freq == 0) {
        freq[i+1] = 0
      } else {
        freq[i+1] = single_freq # exits the  frequency of neuron_id = i for the i neuron
      } 
    }
    
    
    # exits the  frequency of neuron_id = i for the i neuron
  }
  
  mean_freq = mean(freq, na.rm = TRUE)
  error <- qt(0.975,df=length(freq)-1)*sd(freq)/sqrt(length(freq))
  upr = mean(freq) + error
  lwr = mean(freq) - error
  
  
  
  return(data.frame(mean_freq, error, upr, lwr))
  
})

df <- ldply(frequencies, data.frame)

c=1:8

df = cbind(df, data_n=c(50,75,100,125,150)) # number of files you are using, in my case 8 files; 0:255 by 32


ggplot() + 
  # geom_ribbon(data=df, aes(data_n, ymin=lwr, ymax=upr), alpha=0.3, inherit.aes = T) +
  geom_line(data=df, aes(data_n, mean_freq), colour="#D55E00") +
  geom_errorbar(data=df, aes(data_n, ymin=lwr, ymax=upr), colour="black", width=5) +
  geom_point(data=df, aes(data_n, mean_freq), shape=21, size=2, fill="white") +
  labs(x="Stimulus freq [Hz]", y = "Spiking freq [Hz]") +
  labs(title = "Core 1") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("c1_synaptic_eff_mean.png", plot = last_plot(), width=7, height=7)




# /////////////////////////// CORE 2  /////////////////////////////////

frequencies = lapply(files, function(x) {
  file = read.table(x, header = T)
  
  freq= rep(NA, 256)          # create an empty array long 255 full of zeros
  
  for (i in 0:255) {             # loop for the neurons (here total of 256 neurons)
    time_n = file$time[file$chip_id==1 & file$core_id==2 & file$neuron_id==i]  # select the time for different neuron id
    spikes = length(time_n)
    if (spikes==1){
      dtime.s = (max(file$time[file$chip_id==1 & file$core_id==2]) - min(file$time[file$chip_id==1 & file$core_id==2]))/1000000
      single_freq = spikes/(dtime.s)
      
      if (single_freq == 0) {
        freq[i+1] = 0
      } else {
        freq[i+1] = single_freq # exits the  frequency of neuron_id = i for the i neuron
      } 
      
    } else {
      
      dtime.s = (max(file$time[file$chip_id==1 & file$core_id==2]) - min(file$time[file$chip_id==1 & file$core_id==2]))/1000000
      single_freq = spikes/(dtime.s)
      if (single_freq == 0) {
        freq[i+1] = 0
      } else {
        freq[i+1] = single_freq # exits the  frequency of neuron_id = i for the i neuron
      } 
    }
    
    
    # exits the  frequency of neuron_id = i for the i neuron
  }
  
  mean_freq = mean(freq, na.rm = TRUE)
  error <- qt(0.975,df=length(freq)-1)*sd(freq)/sqrt(length(freq))
  upr = mean(freq) + error
  lwr = mean(freq) - error
  
  
  
  return(data.frame(mean_freq, error, upr, lwr))
  
})

df <- ldply(frequencies, data.frame)

c=1:8

df = cbind(df, data_n=c(50,75,100,125,150)) # number of files you are using, in my case 8 files; 0:255 by 32


ggplot() + 
  # geom_ribbon(data=df, aes(data_n, ymin=lwr, ymax=upr), alpha=0.3, inherit.aes = T) +
  geom_line(data=df, aes(data_n, mean_freq), colour="#D55E00") +
  geom_errorbar(data=df, aes(data_n, ymin=lwr, ymax=upr), colour="black", width=5) +
  geom_point(data=df, aes(data_n, mean_freq), shape=21, size=2, fill="white") +
  labs(x="Stimulus freq [Hz]", y = "Spiking freq [Hz]") +
  labs(title = "Core 2") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("c2_synaptic_eff_mean.png", plot = last_plot(), width=7, height=7)






# /////////////////////////// CORE 3  /////////////////////////////////

frequencies = lapply(files, function(x) {
  file = read.table(x, header = T)
  
  freq= rep(NA, 256)          # create an empty array long 255 full of zeros
  
  for (i in 0:255) {             # loop for the neurons (here total of 256 neurons)
    time_n = file$time[file$chip_id==1 & file$core_id==3 & file$neuron_id==i]  # select the time for different neuron id
    spikes = length(time_n)
    if (spikes==1){
      dtime.s = (max(file$time[file$chip_id==1 & file$core_id==3]) - min(file$time[file$chip_id==1 & file$core_id==3]))/1000000
      single_freq = spikes/(dtime.s)
      
      if (single_freq == 0) {
        freq[i+1] = 0
      } else {
        freq[i+1] = single_freq # exits the  frequency of neuron_id = i for the i neuron
      } 
      
    } else {
      
      dtime.s = (max(file$time[file$chip_id==1 & file$core_id==3]) - min(file$time[file$chip_id==1 & file$core_id==3]))/1000000
      single_freq = spikes/(dtime.s)
      if (single_freq == 0) {
        freq[i+1] = 0
      } else {
        freq[i+1] = single_freq # exits the  frequency of neuron_id = i for the i neuron
      } 
    }
    
    
    # exits the  frequency of neuron_id = i for the i neuron
  }
  
  mean_freq = mean(freq, na.rm = TRUE)
  error <- qt(0.975,df=length(freq)-1)*sd(freq)/sqrt(length(freq))
  upr = mean(freq) + error
  lwr = mean(freq) - error
  
  
  
  return(data.frame(mean_freq, error, upr, lwr))
  
})

df <- ldply(frequencies, data.frame)

c=1:8

df = cbind(df, data_n=c(50,75,100,125,150)) # number of files you are using, in my case 8 files; 0:255 by 32


ggplot() + 
  # geom_ribbon(data=df, aes(data_n, ymin=lwr, ymax=upr), alpha=0.3, inherit.aes = T) +
  geom_line(data=df, aes(data_n, mean_freq), colour="#D55E00") +
  geom_errorbar(data=df, aes(data_n, ymin=lwr, ymax=upr), colour="black", width=5) +
  geom_point(data=df, aes(data_n, mean_freq), shape=21, size=2, fill="white") +
  labs(x="Stimulus freq [Hz]", y = "Spiking freq [Hz]") +
  labs(title = "Core 3") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("c3_synaptic_eff_mean.png", plot = last_plot(), width=7, height=7)



end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

