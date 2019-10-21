#R Assignment
###Author: Roshan Kulkarni
###File Information
These files are divided into four folders:
raw\_files, maize\_files, teosinte\_files, plots and the R script is in R\_assignment\_script.r file.

## Data Inspection
###Attributes of **fang\_et\_al\_genotypes**
Here is my snippet of code for the data inspection:
file.info('fang_et_al_genotypes.txt', extra_cols = TRUE)
ncol(Genotypes_file)
nrow(Genotypes_file)

By inspecting this file I learned that:

* Size of fang\_et\_al\_genotypes.txt is 11 MB. 
* Number of columns in fang\_et\_al\_genotypes.txt is 986.

###Attributes of **snp\_position.txt**

Here is my snippet of code for the data inspection:
file.info('snp_position_file', extra_cols = TRUE)
ncol(SNPs_position_file)
nrow(SNPs_position_file)

By inspecting this file I learned that:

* Size of snp\_position.txt is 81K.
* Number of columns in snp\_position.txt is 15.

##Data Processing
###Maize Data

Here is my snippet of code for the data processing:
#### Subsetting Maize and Teosinte file from Genotype_file
maize_data <- Genotypes_file %>% filter(Group %in% c("ZMMIL", "ZMMLR", "ZMMMR"))
teosinte_data <- Genotypes_file %>% filter(Group %in% c("ZMPBA", "ZMPIL", "ZMPJA"))
ncol(maize/_data)
ncol(teosinte/_data)
#### Processing Maize file
#### Transposing Maize data file
maize_transpose_data <- setNames(data.frame(t(maize/_data[-1])), maize/_data[,1])
#### Removing 2nd and 3rd column
maize/_transpose/_data <- maize/_transpose/_data[-c(1,2),]

#### Merging maize file
maize/_merge/_file <- merge(x = SNPs/_position/_file[ , c("Chromosome","Position")], y = maize/_transpose/_data, by.x = 0, by.y = 0, all.y = TRUE)
maize/_merge/_file <- maize/_merge/_file %>% rename("SNPs/_ID" = "Row.names")

#### Subsetting maize file based on individual chr 1-10 & arranging in increasing order of position
Maize/_Chr1 <- maize/_merge/_file %>%
  filter(Chromosome %in% c("1")) %>%
  arrange(as.numeric(Position)) %>%
  write/_csv("Maize/_Chr1.txt")
  
#### Subsetting maize file based on individual chr 1-10 & arranging in decreasing order of position
Maize/_dec/_Chr1 <- maize/_merge/_file %>%
  filter(Chromosome %in% c("1")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write/_csv("Maize/_dec/_Chr1.txt")

#### Subsetting teosinte file based on individual chr 1-10 & arranging in decreasing order of position
Teosinte/_Chr1 <- teosinte/_merge/_file %>%
  filter(Chromosome %in% c("1")) %>%
  arrange(as.numeric(Position)) %>%
  write/_csv("Teosinte/_Chr1.txt")
  
#### Subsetting maize file based on individual chr 1-10 & arranging in decreasing order of position
Teosinte/_dec/_Chr1 <- teosinte/_merge/_file %>%
  filter(Chromosome %in% c("1")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write/_csv("Teosinte/_dec/_Chr1.txt")
  
#### Part2: Data Visualization
#### Transposing Maize data file
genotypes/_transpose/_data <- setNames(data.frame(t(Genotypes/_file[-1])), Genotypes/_file[,1])
##### Removing 2nd and 3rd column
genotypes/_transpose/_data <- genotypes/_transpose/_data[-c(1,2),]
genotypes/_merge/_file <- merge(x = SNPs/_position/_file[ , c("Chromosome","Position")], y = genotypes/_transpose/_data, by.x = 0, by.y = 0, all.y = TRUE)
genotypes/_merge/_file <- genotypes_/merge/_file %>% rename("SNPs/_ID" = "Row.names")
#### Subsetting file for plotting SNPs per chromosome
File/_plot/_count <- count(genotypes/_merge/_file, Chromosome) %>%
  filter(Chromosome %in% c("1","2","3","4","5","6","7","8","9","10"))
#### Plotting SNPs per chromosome 
p/_SNP/_count/_chr <- ggplot(File/_plot/_count, aes(x=Chromosome, y=n, fill=n)) + geom/_bar(stat="identity") + theme/_minimal()
p/_SNP/_count/_chr


#### Subsetting file for plotting distribution of SNPs on chromosomes
File/_plot/_dist <- genotypes/_merge/_file %>%
  select(Chromosome, Position) %>%
  filter(Chromosome %in% c("1","2","3","4","5","6","7","8","9","10")) %>%
  arrange(as.numeric(Position))
  #arrange(as.numeric(Chromosome), as.numeric(Position))

#### PLotting distribution of SNPs on chromosomes
P/_SNP/_dist <- ggplot(File/_plot/_dist, aes(x=Chromosome, y=Position)) + geom/_point(stat = "identity", position = "identity", size=1, color = "blue")
P/_SNP/_dist + theme(axis.text.y=element/_blank())
#### Preparing file for plotting proportion of homozygous and heterozygous SNPs
Genotypes/_file_new <- Genotypes/_file %>%
  mutate/_if(is.character, str/_replace/_all, pattern = "A/A", replacement = "Homozygous") %>%
  mutate/_if(is.character, str/_replace/_all, pattern = "T/T", replacement = "Homozygous") %>%
  mutate/_if(is.character, str/_replace_all, pattern = "G/G", replacement = "Homozygous") %>%
  mutate/_if(is.character, str/_replace_all, pattern = "C/C", replacement = "Homozygous") %>%
  mutate/_if(is.character, str/_replace_all, pattern = "A/T", replacement = "Heterozygous") %>%
  mutate/_if(is.character, str/_replace_all, pattern = "A/G", replacement = "Heterozygous") %>%
  mutate/_if(is.character, str/_replace_all, pattern = "A/C", replacement = "Heterozygous") %>%
  mutate/_if(is.character, str/_replace_all, pattern = "T/A", replacement = "Heterozygous") %>%
  mutate/_if(is.character, str/_replace_all, pattern = "T/G", replacement = "Heterozygous") %>%
  mutate/_if(is.character, str/_replace_all, pattern = "T/C", replacement = "Heterozygous") %>%
  mutate/_if(is.character, str/_replace_all, pattern = "G/A", replacement = "Heterozygous") %>%
  mutate/_if(is.character, str/_replace_all, pattern = "G/T", replacement = "Heterozygous") %>%
  mutate/_if(is.character, str_replace/_all, pattern = "G/C", replacement = "Heterozygous") %>%
  mutate/_if(is.character, str_replace/_all, pattern = "C/A", replacement = "Heterozygous") %>%
  mutate/_if(is.character, str_replace/_all, pattern = "C/T", replacement = "Heterozygous") %>%
  mutate/_if(is.character, str/_replace_all, pattern = "C/G", replacement = "Heterozygous") %>%
  mutate/_if(is.character, str/_replace_all, pattern = "\\?\\?", replacement = "Missing")

####Subsetting SNP columns
MyColname <- colnames(Genotypes/_file/_new)[-c(1:3)]
Genotypes/_file/_new/_long <- gather(Genotypes/_file/_new, SNP, Genotype, MyColname)

#### Plotting the proportion heterozygous and homozygous SNPs with missing data
Plot/_homo/_het <- ggplot(data = Genotypes/_file/_new/_long) + geom/_bar(mapping = aes(x = Sample/_ID, fill=Genotype))
Plot/_homo/_het + theme(axis.text.x = element/_blank())

#### Plotting the proportion heterozygous and homozygous SNPs with missing data across groups
Plot/_homo/_het <- ggplot(data = Genotypes/_file/_new/_long) + geom/_bar(mapping = aes(x = Group, fill=Genotype))
Plot/_homo/_het + theme(axis.text.x = element/_blank())

