# activate library for ggplot ; if you don't have it -> install.packages("ggplot2")
library(ggplot2)
library(plyr)
library(reshape2)
library(grid)

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



# //////////////////// CORE 0  ///////////////////////
# ====================================================


frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
  
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)
  
  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==1 & file$core_id==0 & file$neuron_id==i]
    spikes = length(neuron_time)
    
    
    if (spikes==1){
      dtime.s = (max(file$time[file$chip_id==1 & file$core_id==0]) - min(file$time[file$chip_id==1 & file$core_id==0]))/1000000
      single_freq = spikes/(dtime.s)
      freq[i+1] = single_freq # exits the  frequency of neuron_id = i for the i neuron
      
    } else {
      dtime.s = (max(file$time[file$chip_id==1 & file$core_id==0]) - min(file$time[file$chip_id==1 & file$core_id==0]))/1000000
      single_freq = spikes/(dtime.s)
      
      if (single_freq == 0) {
        freq[i+1] = 0
      } else {
        freq[i+1] = single_freq # exits the  frequency of neuron_id = i for the i neuron
      } 
    }
  }
  
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:5), sep="")

df = melt(frequencies)

# AAA CHANGE HERE THE FREQUENCIES USED

df <- cbind(df, freq = c(rep(50,256), rep(75,256), rep(100, 256), rep(125,256), rep(150,256)))

df <- cbind(df, synapticE = df$value / df$freq )

df$freq <- factor(df$freq)

for (i in 1:5) {
  
  grp.mean = mean(df$synapticE[df$Var2==i])
  grp.mean = round(grp.mean, 6)
  
  sd1 = sd(df$synapticE[df$Var2==i])
  sd1 = round(sd1, 6)
  
  grob <- grobTree(textGrob(label = paste("Mean:", grp.mean), x=0.8,  y=0.95, hjust=0,
                            gp=gpar(col="black", fontsize=9)))
  grob2 <- grobTree(textGrob(label = paste("Std.Dev:", sd1), x=0.8,  y=0.90, hjust=0,
                             gp=gpar(col="black", fontsize=9)))
  
  ggplot(data=subset(df, Var2==i), aes(synapticE)) + 
    geom_histogram(binwidth=0.025, col="black",
                   aes(fill=..count..)) +
    scale_fill_gradient("Count", low = "blue", high = "red") +
    
    geom_vline(data=subset(df, Var2=i), aes(xintercept=grp.mean),
               linetype="dashed") +
    
    geom_vline(data=subset(df, Var2==i), aes(xintercept=grp.mean+sd1),
               linetype="1F")+
    geom_vline(data=subset(df, Var2==i), aes(xintercept=grp.mean-sd1),
               linetype="1F") +
    
    theme(legend.position="none") +
    labs(x="Synaptic Efficacy", y = "Count") +
    labs(title = paste("Core 0 - ", df$freq[df$Var2==i], " Hz", sep="")) + theme(plot.title = element_text(hjust = 0.5, size=rel(2))) +
    annotation_custom(grob) + annotation_custom(grob2)
  
  ggsave(filename=paste("c0_synaptic_eff", i,".png",sep=""), plot = last_plot(), width=7, height=7)
}




# //////////////////// CORE 1  ///////////////////////
# ====================================================


frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
  
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)
  
  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==1 & file$core_id==1 & file$neuron_id==i]
    spikes = length(neuron_time)
    
    
    if (spikes==1){
      dtime.s = (max(file$time[file$chip_id==1 & file$core_id==1]) - min(file$time[file$chip_id==1 & file$core_id==1]))/1000000
      single_freq = spikes/(dtime.s)
      freq[i+1] = single_freq # exits the  frequency of neuron_id = i for the i neuron
      
    } else {
      dtime.s = (max(file$time[file$chip_id==1 & file$core_id==1]) - min(file$time[file$chip_id==1 & file$core_id==1]))/1000000
      single_freq = spikes/(dtime.s)
      
      if (single_freq == 0) {
        freq[i+1] = 0
      } else {
        freq[i+1] = single_freq # exits the  frequency of neuron_id = i for the i neuron
      } 
    }
  }
  
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:5), sep="")

df = melt(frequencies)

# AAA CHANGE HERE THE FREQUENCIES USED

df <- cbind(df, freq = c(rep(50,256), rep(75,256), rep(100, 256), rep(125,256), rep(150,256)))

df <- cbind(df, synapticE = df$value / df$freq )

df$freq <- factor(df$freq)

for (i in 1:5) {
  
  grp.mean = mean(df$synapticE[df$Var2==i])
  grp.mean = round(grp.mean, 6)
  
  sd1 = sd(df$synapticE[df$Var2==i])
  sd1 = round(sd1, 6)
  
  grob <- grobTree(textGrob(label = paste("Mean:", grp.mean), x=0.8,  y=0.95, hjust=0,
                            gp=gpar(col="black", fontsize=9)))
  grob2 <- grobTree(textGrob(label = paste("Std.Dev:", sd1), x=0.8,  y=0.90, hjust=0,
                             gp=gpar(col="black", fontsize=9)))
  
  ggplot(data=subset(df, Var2==i), aes(synapticE)) + 
    geom_histogram(binwidth=0.025, col="black",
                   aes(fill=..count..)) +
    scale_fill_gradient("Count", low = "blue", high = "red") +
    
    geom_vline(data=subset(df, Var2=i), aes(xintercept=grp.mean),
               linetype="dashed") +
    
    geom_vline(data=subset(df, Var2==i), aes(xintercept=grp.mean+sd1),
               linetype="1F")+
    geom_vline(data=subset(df, Var2==i), aes(xintercept=grp.mean-sd1),
               linetype="1F") +
    
    theme(legend.position="none") +
    labs(x="Synaptic Efficacy", y = "Count") +
    labs(title = paste("Core 1 - ", df$freq[df$Var2==i], " Hz", sep="")) + theme(plot.title = element_text(hjust = 0.5, size=rel(2))) +
    annotation_custom(grob) + annotation_custom(grob2)
  
  ggsave(filename=paste("c1_synaptic_eff", i,".png",sep=""), plot = last_plot(), width=7, height=7)
}



# //////////////////// CORE 2  ///////////////////////
# ====================================================


frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
  
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)
  
  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==1 & file$core_id==2 & file$neuron_id==i]
    spikes = length(neuron_time)
    
    
    if (spikes==1){
      dtime.s = (max(file$time[file$chip_id==1 & file$core_id==2]) - min(file$time[file$chip_id==1 & file$core_id==2]))/1000000
      single_freq = spikes/(dtime.s)
      freq[i+1] = single_freq # exits the  frequency of neuron_id = i for the i neuron
      
    } else {
      dtime.s = (max(file$time[file$chip_id==1 & file$core_id==2]) - min(file$time[file$chip_id==1 & file$core_id==2]))/1000000
      single_freq = spikes/(dtime.s)
      
      if (single_freq == 0) {
        freq[i+1] = 0
      } else {
        freq[i+1] = single_freq # exits the  frequency of neuron_id = i for the i neuron
      } 
    }
  }
  
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:5), sep="")

df = melt(frequencies)

# AAA CHANGE HERE THE FREQUENCIES USED

df <- cbind(df, freq = c(rep(50,256), rep(75,256), rep(100, 256), rep(125,256), rep(150,256)))

df <- cbind(df, synapticE = df$value / df$freq )

df$freq <- factor(df$freq)

for (i in 1:5) {
  
  grp.mean = mean(df$synapticE[df$Var2==i])
  grp.mean = round(grp.mean, 6)
  
  sd1 = sd(df$synapticE[df$Var2==i])
  sd1 = round(sd1, 6)
  
  grob <- grobTree(textGrob(label = paste("Mean:", grp.mean), x=0.8,  y=0.95, hjust=0,
                            gp=gpar(col="black", fontsize=9)))
  grob2 <- grobTree(textGrob(label = paste("Std.Dev:", sd1), x=0.8,  y=0.90, hjust=0,
                             gp=gpar(col="black", fontsize=9)))
  
  ggplot(data=subset(df, Var2==i), aes(synapticE)) + 
    geom_histogram(binwidth=0.025, col="black",
                   aes(fill=..count..)) +
    scale_fill_gradient("Count", low = "blue", high = "red") +
    
    geom_vline(data=subset(df, Var2=i), aes(xintercept=grp.mean),
               linetype="dashed") +
    
    geom_vline(data=subset(df, Var2==i), aes(xintercept=grp.mean+sd1),
               linetype="1F")+
    geom_vline(data=subset(df, Var2==i), aes(xintercept=grp.mean-sd1),
               linetype="1F") +
    
    theme(legend.position="none") +
    labs(x="Synaptic Efficacy", y = "Count") +
    labs(title = paste("Core 2 - ", df$freq[df$Var2==i], " Hz", sep="")) + theme(plot.title = element_text(hjust = 0.5, size=rel(2))) +
    annotation_custom(grob) + annotation_custom(grob2)
  
  ggsave(filename=paste("c2_synaptic_eff", i,".png",sep=""), plot = last_plot(), width=7, height=7)
}




# //////////////////// CORE 3  ///////////////////////
# ====================================================


frequencies = sapply(files, function(x) {
  file <- read.table(x, header = T)
  
  neurons = data.frame()
  neurons = rbind(0:255)
  
  freq = rep(NA, 256)
  
  for (i in 0:255) {
    neuron_time =  file$time[file$chip_id==1 & file$core_id==3 & file$neuron_id==i]
    spikes = length(neuron_time)
    
    
    if (spikes==1){
      dtime.s = (max(file$time[file$chip_id==1 & file$core_id==3]) - min(file$time[file$chip_id==1 & file$core_id==3]))/1000000
      single_freq = spikes/(dtime.s)
      freq[i+1] = single_freq # exits the  frequency of neuron_id = i for the i neuron
      
    } else {
      dtime.s = (max(file$time[file$chip_id==1 & file$core_id==3]) - min(file$time[file$chip_id==1 & file$core_id==3]))/1000000
      single_freq = spikes/(dtime.s)
      
      if (single_freq == 0) {
        freq[i+1] = 0
      } else {
        freq[i+1] = single_freq # exits the  frequency of neuron_id = i for the i neuron
      } 
    }
  }
  
  
  return(freq)
  
})

# renaming the columns with the fineValues we used (31, 63, 95, ...)
colnames(frequencies) = paste("",(1:5), sep="")

df = melt(frequencies)

# AAA CHANGE HERE THE FREQUENCIES USED

df <- cbind(df, freq = c(rep(50,256), rep(75,256), rep(100, 256), rep(125,256), rep(150,256)))

df <- cbind(df, synapticE = df$value / df$freq )

df$freq <- factor(df$freq)

for (i in 1:5) {
  
  grp.mean = mean(df$synapticE[df$Var2==i])
  grp.mean = round(grp.mean, 6)
  
  sd1 = sd(df$synapticE[df$Var2==i])
  sd1 = round(sd1, 6)
  
  grob <- grobTree(textGrob(label = paste("Mean:", grp.mean), x=0.8,  y=0.95, hjust=0,
                            gp=gpar(col="black", fontsize=9)))
  grob2 <- grobTree(textGrob(label = paste("Std.Dev:", sd1), x=0.8,  y=0.90, hjust=0,
                             gp=gpar(col="black", fontsize=9)))
  
  ggplot(data=subset(df, Var2==i), aes(synapticE)) + 
    geom_histogram(binwidth=0.025, col="black",
                   aes(fill=..count..)) +
    scale_fill_gradient("Count", low = "blue", high = "red") +
    
    geom_vline(data=subset(df, Var2=i), aes(xintercept=grp.mean),
               linetype="dashed") +
    
    geom_vline(data=subset(df, Var2==i), aes(xintercept=grp.mean+sd1),
               linetype="1F")+
    geom_vline(data=subset(df, Var2==i), aes(xintercept=grp.mean-sd1),
               linetype="1F") +
    
    theme(legend.position="none") +
    labs(x="Synaptic Efficacy", y = "Count") +
    labs(title = paste("Core 3 - ", df$freq[df$Var2==i], " Hz", sep="")) +
    theme(plot.title = element_text(hjust = 0.5, size=rel(2))) +
    annotation_custom(grob) + annotation_custom(grob2)
  
  ggsave(filename=paste("c3_synaptic_eff", i,".png",sep=""), plot = last_plot(), width=7, height=7)
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken




