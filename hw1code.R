## To run code from terminal
  # RScript hw1code.R input-file descriptor-file key k output-file
  # input-file = input data file
  # descriptor-file = file where column [1] contains attribute names & col [2] identifies quasi-identifiers
  # key = file where column [1] contains original values & col [2] contains generalizations of values
  # k = desired value of k
  # output-file = name of file to export

# Convert command line input to variables 
args = commandArgs(trailingOnly = TRUE) 
input = args[1] 
des = args[2] 
key = args[3]
k = as.numeric(args[4]) 
output = args[5] 

# Install dplyr if missing
if(!"dplyr"%in%installed.packages()){
  install.packages("dplyr")
}

# Attach dplyr package
library(dplyr)

# Import input data
data = read.table(input)

# Import descriptor file
names = read.table(des, header = T)

# Add attribute names to input data-frame
colnames(data) = names[,1]

# Select the quasi-identifiers
QI = as.character(names[names$isQI==T,1])

# Import the hierarchy value key
dfkey = read.table(key, header = T)

# Generalize attributes/suppress tuples
repeat{
  
  freqC = data %>% group_by_at(QI) %>% count() # Get frequency counts of unique QI combinations
  
  if( all(freqC$n >=k) ){ # Break loop if data is k-anonymous
    
    print("Data is anonymous. Breaking loop...")
    datanew=data
    break
    
  } else if ( sum( freqC$n[freqC$n < k] ) <= k){ # Break loop if # tuples to be suppressed <= k
    
    print("Suppressing...")
    index = data %>% group_by_at(QI) %>% mutate(group = group_indices()) %>% as.data.frame()
    freqG = freqC %>% mutate(group = group_indices()) %>% as.data.frame()
    drop = freqG[freqG$n<k, "group"]
    datanew = index[!index$group%in%drop,] %>% select(-group)
    break
    
  } else { #  If data is not k-anonymous/can't be suppressed, generalize attribute with most unqiue values
    
    print("Generalizing...")
    maxC = sapply(freqC[,-ncol(freqC)], n_distinct) # Count unqiue values per attribute
    Attr = which.max(maxC) %>% names() # Note: if duplicate max values, selects in order of appearance
    data[,Attr] <- dfkey[,2][match(data[,Attr], dfkey[,1])]
    
  }
  
}

# Write data to new file
write.table(datanew, file = paste0(output, ".txt"), row.names = F, col.names = T, sep = "\t", quote = F)

# Optional command to return frequency table of unique QI combinations. Uncomment to run
write.table(freqC, file = "freq.txt", row.names = F, col.names = T, sep = "\t", quote = F)






