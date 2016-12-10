#  
#  
#           /// Visualisation of frequencies changes relative to current input variation 
#                   in Dynap-se Neuromorphic chip's 256 neurons (of the four cores)    ///
# 
# 
#       This script can analyze the data extracted from the .aedat files recorded with
#       cAER, by using the python script found in the repository 
#       (https://github.com/AdenosinTP/project_ETH), and show the frequency of each 
#       neuron for an increasing current (in this case DC coarseValue was costant and
#         changes were in fineValue with the rest of the parameters unchanged from )
# 
# 
#                 Author : Valerio Tettamanti valerite@student.ethz.ch


###########################################################################################


# First, go on session -> set working directory; choose directory 
# select the folder with your .txt files


# activate library for ggplot ; if you don't have it -> install.packages("ggplot2")
library(ggplot2)
library(plyr)
library(reshape2)

start.time <- Sys.time()

# Reading all the files *.txt
files = list.files( pattern = "*.txt")
n = length(files)

# extract the time of each spike (for each one of the 256 neurons) and calculate
# the frequency for each current change


# ===========================================================================
# ===========================================================================
# //////////////////////////////   CHIP 0    ////////////////////////////////
# ===========================================================================
# ===========================================================================



# //////////////////// CORE 0 - CHIP 0 ///////////////////////
#  ===========================================================


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
    freq_single = spikes/(dtime)
    freq[i+1] = freq_single 
  }
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:8)*32-1, sep="")


df = melt(frequencies)

# plottig the frequencies changes for 256 neurons
ggplot(df, aes(Var2, value, group=factor(Var1))) + geom_line(aes(color=factor(Var1))) + 
  theme(legend.position="none") +
  labs(x="fValue", y = "Frequency [Hz]") +
  labs(title = "Chip 0 - Core 0") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("neurons_chip_0_core_0.png", plot = last_plot())






# //////////////////// CORE 1 - CHIP 0  ///////////////////////
# =============================================================


frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
  
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)
  
  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==1 & file$core_id==1 & file$neuron_id==i]
    spikes = length(neuron_time)
    min = min(neuron_time)
    max = max(neuron_time)
    dtime = (max - min)/1000000
    freq_single = spikes/(dtime)
    freq[i+1] = freq_single 
  }
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:8)*32-1, sep="")


df = melt(frequencies)


# plotting the frequencies changes for 256 neurons
ggplot(df, aes(Var2, value, group=factor(Var1))) + geom_line(aes(color=factor(Var1))) + 
  theme(legend.position="none") +
  labs(x="fValue", y = "Frequency [Hz]") +
  labs(title = "Chip 0 - Core 1") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("neurons_chip_0_core_1.png", plot = last_plot())






# //////////////////// CORE 2 - CHIP 0 ///////////////////////
# ============================================================


frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
  
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)
  
  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==1 & file$core_id==2 & file$neuron_id==i]
    spikes = length(neuron_time)
    min = min(neuron_time)
    max = max(neuron_time)
    dtime = (max - min)/1000000
    freq_single = spikes/(dtime)
    freq[i+1] = freq_single 
  }
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:8)*32-1, sep="")


df = melt(frequencies)
names(df)

# plottig the frequencies changes for 256 neurons
ggplot(df, aes(Var2, value, group=factor(Var1))) + geom_line(aes(color=factor(Var1))) + 
  theme(legend.position="none") +
  labs(x="fValue", y = "Frequency [Hz]") +
  labs(title = "Chip 0 - Core 2") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("neurons_chip_0_core_2.png", plot = last_plot())






# //////////////////// CORE 3 - CHIP 0 ///////////////////////
# ============================================================


frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
  
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)
  
  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==1 & file$core_id==3 & file$neuron_id==i]
    spikes = length(neuron_time)
    min = min(neuron_time)
    max = max(neuron_time)
    dtime = (max - min)/1000000
    freq_single = spikes/(dtime)
    freq[i+1] = freq_single 
  }
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:8)*32-1, sep="")


df = melt(frequencies)

# plotting the frequencies changes for 256 neurons
ggplot(df, aes(Var2, value, group=factor(Var1))) + geom_line(aes(color=factor(Var1))) + 
  theme(legend.position="none") +
  labs(x="fValue", y = "Frequency [Hz]") +
  labs(title = "Chip 0 - Core 3") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("neurons_chip_0_core_3.png", plot = last_plot())





# ===========================================================================
# ===========================================================================
# //////////////////////////////   CHIP 1    ////////////////////////////////
# ===========================================================================
# ===========================================================================



# //////////////////// CORE 0 - CHIP 1 ///////////////////////
# ============================================================


frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
  
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)
  
  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==4 & file$core_id==0 & file$neuron_id==i]
    spikes = length(neuron_time)
    min = min(neuron_time)
    max = max(neuron_time)
    dtime = (max - min)/1000000
    freq_single = spikes/(dtime)
    freq[i+1] = freq_single 
  }
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:8)*32-1, sep="")


df = melt(frequencies)

# plottig the frequencies changes for 256 neurons
ggplot(df, aes(Var2, value, group=factor(Var1))) + geom_line(aes(color=factor(Var1))) + 
  theme(legend.position="none") +
  labs(x="fValue", y = "Frequency [Hz]") +
  labs(title = "Chip 1 - Core 0") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("neurons_chip_1_core_0.png", plot = last_plot())






# //////////////////// CORE 1 - CHIP 1 ///////////////////////
# ============================================================


frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
  
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)
  
  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==4 & file$core_id==1 & file$neuron_id==i]
    spikes = length(neuron_time)
    min = min(neuron_time)
    max = max(neuron_time)
    dtime = (max - min)/1000000
    freq_single = spikes/(dtime)
    freq[i+1] = freq_single 
  }
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:8)*32-1, sep="")


df = melt(frequencies)


# plotting the frequencies changes for 256 neurons
ggplot(df, aes(Var2, value, group=factor(Var1))) + geom_line(aes(color=factor(Var1))) + 
  theme(legend.position="none") +
  labs(x="fValue", y = "Frequency [Hz]") +
  labs(title = "Chip 1 - Core 1") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("neurons_chip_1_core_1.png", plot = last_plot())






# //////////////////// CORE 2 - CHIP 1 ///////////////////////
# ============================================================


frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
  
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)
  
  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==4 & file$core_id==2 & file$neuron_id==i]
    spikes = length(neuron_time)
    min = min(neuron_time)
    max = max(neuron_time)
    dtime = (max - min)/1000000
    freq_single = spikes/(dtime)
    freq[i+1] = freq_single 
  }
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:8)*32-1, sep="")


df = melt(frequencies)
names(df)

# plottig the frequencies changes for 256 neurons
ggplot(df, aes(Var2, value, group=factor(Var1))) + geom_line(aes(color=factor(Var1))) + 
  theme(legend.position="none") +
  labs(x="fValue", y = "Frequency [Hz]") +
  labs(title = "Chip 1 - Core 2") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("neurons_chip_1_core_2.png", plot = last_plot())






# //////////////////// CORE 3 - CHIP 1 ///////////////////////
# ============================================================


frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
  
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)
  
  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==4 & file$core_id==3 & file$neuron_id==i]
    spikes = length(neuron_time)
    min = min(neuron_time)
    max = max(neuron_time)
    dtime = (max - min)/1000000
    freq_single = spikes/(dtime)
    freq[i+1] = freq_single 
  }
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:8)*32-1, sep="")


df = melt(frequencies)

# plotting the frequencies changes for 256 neurons
ggplot(df, aes(Var2, value, group=factor(Var1))) + geom_line(aes(color=factor(Var1))) + 
  theme(legend.position="none") +
  labs(x="fValue", y = "Frequency [Hz]") +
  labs(title = "Chip 1 - Core 3") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("neurons_chip_1_core_3.png", plot = last_plot())





# ===========================================================================
# ===========================================================================
# //////////////////////////////   CHIP 2    ////////////////////////////////
# ===========================================================================
# ===========================================================================



# //////////////////// CORE 0 - CHIP 2 ///////////////////////
# ============================================================


frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
  
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)
  
  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==8 & file$core_id==0 & file$neuron_id==i]
    spikes = length(neuron_time)
    min = min(neuron_time)
    max = max(neuron_time)
    dtime = (max - min)/1000000
    freq_single = spikes/(dtime)
    freq[i+1] = freq_single 
  }
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:8)*32-1, sep="")


df = melt(frequencies)

# plottig the frequencies changes for 256 neurons
ggplot(df, aes(Var2, value, group=factor(Var1))) + geom_line(aes(color=factor(Var1))) + 
  theme(legend.position="none") +
  labs(x="fValue", y = "Frequency [Hz]") +
  labs(title = "Chip 2 - Core 0") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("neurons_chip_2_core_0.png", plot = last_plot())






# //////////////////// CORE 1 - CHIP 2 ///////////////////////
# ============================================================


frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
  
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)
  
  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==8 & file$core_id==1 & file$neuron_id==i]
    spikes = length(neuron_time)
    min = min(neuron_time)
    max = max(neuron_time)
    dtime = (max - min)/1000000
    freq_single = spikes/(dtime)
    freq[i+1] = freq_single 
  }
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:8)*32-1, sep="")


df = melt(frequencies)


# plotting the frequencies changes for 256 neurons
ggplot(df, aes(Var2, value, group=factor(Var1))) + geom_line(aes(color=factor(Var1))) + 
  theme(legend.position="none") +
  labs(x="fValue", y = "Frequency [Hz]") +
  labs(title = "Chip 2 - Core 1") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("neurons_chip_2_core_1.png", plot = last_plot())






# //////////////////// CORE 2 - CHIP 2 ///////////////////////
# ============================================================


frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
  
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)
  
  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==8 & file$core_id==2 & file$neuron_id==i]
    spikes = length(neuron_time)
    min = min(neuron_time)
    max = max(neuron_time)
    dtime = (max - min)/1000000
    freq_single = spikes/(dtime)
    freq[i+1] = freq_single 
  }
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:8)*32-1, sep="")


df = melt(frequencies)
names(df)

# plottig the frequencies changes for 256 neurons
ggplot(df, aes(Var2, value, group=factor(Var1))) + geom_line(aes(color=factor(Var1))) + 
  theme(legend.position="none") +
  labs(x="fValue", y = "Frequency [Hz]") +
  labs(title = "Chip 2 - Core 2") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("neurons_chip_2_core_2.png", plot = last_plot())






# //////////////////// CORE 3 - CHIP 2 ///////////////////////
# ============================================================


frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
  
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)
  
  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==8 & file$core_id==3 & file$neuron_id==i]
    spikes = length(neuron_time)
    min = min(neuron_time)
    max = max(neuron_time)
    dtime = (max - min)/1000000
    freq_single = spikes/(dtime)
    freq[i+1] = freq_single 
  }
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:8)*32-1, sep="")


df = melt(frequencies)

# plotting the frequencies changes for 256 neurons
ggplot(df, aes(Var2, value, group=factor(Var1))) + geom_line(aes(color=factor(Var1))) + 
  theme(legend.position="none") +
  labs(x="fValue", y = "Frequency [Hz]") +
  labs(title = "Chip 2 - Core 3") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("neurons_chip_2_core_3.png", plot = last_plot())





# ===========================================================================
# ===========================================================================
# //////////////////////////////   CHIP 3    ////////////////////////////////
# ===========================================================================
# ===========================================================================



# //////////////////// CORE 0 - CHIP 3 ///////////////////////
# ============================================================


frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
  
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)
  
  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==12 & file$core_id==0 & file$neuron_id==i]
    spikes = length(neuron_time)
    min = min(neuron_time)
    max = max(neuron_time)
    dtime = (max - min)/1000000
    freq_single = spikes/(dtime)
    freq[i+1] = freq_single 
  }
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:8)*32-1, sep="")


df = melt(frequencies)

# plottig the frequencies changes for 256 neurons
ggplot(df, aes(Var2, value, group=factor(Var1))) + geom_line(aes(color=factor(Var1))) + 
  theme(legend.position="none") +
  labs(x="fValue", y = "Frequency [Hz]") +
  labs(title = "Chip 3 - Core 0") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("neurons_chip_3_core_0.png", plot = last_plot())






# //////////////////// CORE 1 - CHIP 3 ///////////////////////
# ============================================================


frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
  
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)
  
  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==12 & file$core_id==1 & file$neuron_id==i]
    spikes = length(neuron_time)
    min = min(neuron_time)
    max = max(neuron_time)
    dtime = (max - min)/1000000
    freq_single = spikes/(dtime)
    freq[i+1] = freq_single 
  }
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:8)*32-1, sep="")


df = melt(frequencies)


# plotting the frequencies changes for 256 neurons
ggplot(df, aes(Var2, value, group=factor(Var1))) + geom_line(aes(color=factor(Var1))) + 
  theme(legend.position="none") +
  labs(x="fValue", y = "Frequency [Hz]") +
  labs(title = "Chip 3 - Core 1") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("neurons_chip_3_core_1.png", plot = last_plot())






# //////////////////// CORE 2 - CHIP 3 ///////////////////////
# ============================================================


frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
  
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)
  
  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==12 & file$core_id==2 & file$neuron_id==i]
    spikes = length(neuron_time)
    min = min(neuron_time)
    max = max(neuron_time)
    dtime = (max - min)/1000000
    freq_single = spikes/(dtime)
    freq[i+1] = freq_single 
  }
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:8)*32-1, sep="")


df = melt(frequencies)
names(df)

# plottig the frequencies changes for 256 neurons
ggplot(df, aes(Var2, value, group=factor(Var1))) + geom_line(aes(color=factor(Var1))) + 
  theme(legend.position="none") +
  labs(x="fValue", y = "Frequency [Hz]") +
  labs(title = "Chip 3 - Core 2") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("neurons_chip_3_core_2.png", plot = last_plot())






# //////////////////// CORE 3 - CHIP 3 ///////////////////////
# ============================================================


frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
  
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)
  
  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==12 & file$core_id==3 & file$neuron_id==i]
    spikes = length(neuron_time)
    min = min(neuron_time)
    max = max(neuron_time)
    dtime = (max - min)/1000000
    freq_single = spikes/(dtime)
    freq[i+1] = freq_single 
  }
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:8)*32-1, sep="")


df = melt(frequencies)

# plotting the frequencies changes for 256 neurons
ggplot(df, aes(Var2, value, group=factor(Var1))) + geom_line(aes(color=factor(Var1))) + 
  theme(legend.position="none") +
  labs(x="fValue", y = "Frequency [Hz]") +
  labs(title = "Chip 3 - Core 3") + theme(plot.title = element_text(hjust = 0.5, size=rel(2)))

ggsave("neurons_chip_3_core_3.png", plot = last_plot())

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken




