# Installing necessary packages
library(dplyr)
library(plyr)
library(ggplot2)
library(tidyverse)

#### Part1
### Opening the raw inout files
# Open the genotype file
Genotypes_file <- read.csv('fang_et_al_genotypes.txt', header = TRUE, sep = "\t", stringsAsFactors = FALSE)
# Open the SNPs_position file
SNPs_position_file <- read.csv('snp_position.txt', header=TRUE, fill = TRUE, sep = "\t", stringsAsFactors = FALSE)
rownames(SNPs_position_file) <- SNPs_position_file$SNP_ID
SNPs_position_file$SNP_ID <- NULL
#View(SNPs_position_file)

### Data Inspection
# Getting the file info
file.info('fang_et_al_genotypes.txt', extra_cols = TRUE)
file.info('snp_position_file', extra_cols = TRUE)

# Calculating number of columns and rows
ncol(Genotypes_file)
nrow(Genotypes_file)
ncol(SNPs_position_file)
nrow(SNPs_position_file)

### Data Processing
# Subsetting Maize and Teosinte file from Genotype_file
maize_data <- Genotypes_file %>% filter(Group %in% c("ZMMIL", "ZMMLR", "ZMMMR"))
teosinte_data <- Genotypes_file %>% filter(Group %in% c("ZMPBA", "ZMPIL", "ZMPJA"))
ncol(maize_data)
ncol(teosinte_data)
## Processing Maize file
# Transposing Maize data file
maize_transpose_data <- setNames(data.frame(t(maize_data[-1])), maize_data[,1])
# Removing 2nd and 3rd column
maize_transpose_data <- maize_transpose_data[-c(1,2),]
#View(maize_transpose_data)
# Merging maize file
maize_merge_file <- merge(x = SNPs_position_file[ , c("Chromosome","Position")], y = maize_transpose_data, by.x = 0, by.y = 0, all.y = TRUE)
maize_merge_file <- maize_merge_file %>% rename("SNPs_ID" = "Row.names")
#View(maize_merge_file)

# Subsetting maize file based on individual chr 1-10 & arranging in increasing order of position
Maize_Chr1 <- maize_merge_file %>%
  filter(Chromosome %in% c("1")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Maize_Chr1.txt")


Maize_Chr2 <- maize_merge_file %>%
  filter(Chromosome %in% c("2")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Maize_Chr2.txt")

Maize_Chr3 <- maize_merge_file %>%
  filter(Chromosome %in% c("3")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Maize_Chr3.txt")

Maize_Chr4 <- maize_merge_file %>%
  filter(Chromosome %in% c("4")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Maize_Chr4.txt")

Maize_Chr5 <- maize_merge_file %>%
  filter(Chromosome %in% c("5")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Maize_Chr5.txt")

Maize_Chr6 <- maize_merge_file %>%
  filter(Chromosome %in% c("6")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Maize_Chr6.txt")

Maize_Chr7 <- maize_merge_file %>%
  filter(Chromosome %in% c("7")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Maize_Chr7.txt")

Maize_Chr8 <- maize_merge_file %>%
  filter(Chromosome %in% c("8")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Maize_Chr8.txt")

Maize_Chr9 <- maize_merge_file %>%
  filter(Chromosome %in% c("9")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Maize_Chr9.txt")

Maize_Chr10 <- maize_merge_file %>%
  filter(Chromosome %in% c("10")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Maize_Chr10.txt")

# Subsetting maize file based on individual chr 1-10 & arranging in decreasing order of position
Maize_dec_Chr1 <- maize_merge_file %>%
  filter(Chromosome %in% c("1")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Maize_dec_Chr1.txt")
#View(Maize_dec_Chr1)

Maize_dec_Chr2 <- maize_merge_file %>%
  filter(Chromosome %in% c("2")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Maize_dec_Chr2.txt")

Maize_dec_Chr3 <- maize_merge_file %>%
  filter(Chromosome %in% c("3")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Maize_dec_Chr3.txt")

Maize_dec_Chr4 <- maize_merge_file %>%
  filter(Chromosome %in% c("4")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Maize_dec_Chr4.txt")

Maize_dec_Chr5 <- maize_merge_file %>%
  filter(Chromosome %in% c("5")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Maize_dec_Chr5.txt")

Maize_dec_Chr6 <- maize_merge_file %>%
  filter(Chromosome %in% c("6")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Maize_dec_Chr6.txt")

Maize_dec_Chr7 <- maize_merge_file %>%
  filter(Chromosome %in% c("7")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Maize_dec_Chr7.txt")

Maize_dec_Chr8 <- maize_merge_file %>%
  filter(Chromosome %in% c("8")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Maize_dec_Chr8.txt")

Maize_dec_Chr9 <- maize_merge_file %>%
  filter(Chromosome %in% c("9")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Maize_dec_Chr9.txt")

Maize_dec_Chr10 <- maize_merge_file %>%
  filter(Chromosome %in% c("10")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Maize_dec_Chr10.txt")

## Processing Teosinte file
# Transposing Teosinte data file
teosinte_transpose_data <- setNames(data.frame(t(teosinte_data[-1])), teosinte_data[,1])
# Removing 2nd and 3rd column
teosinte_transpose_data <- teosinte_transpose_data[-c(1,2),]
#View(teosinte_transpose_data)
# Merging maize file
teosinte_merge_file <- merge(x = SNPs_position_file[ , c("Chromosome","Position")], y = teosinte_transpose_data, by.x = 0, by.y = 0, all.y = TRUE)
teosinte_merge_file <- teosinte_merge_file %>% rename("SNPs_ID" = "Row.names")
#View(teosinte_merge_file)

# Subsetting teosinte file based on individual chr 1-10 & arranging in decreasing order of position
Teosinte_Chr1 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("1")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Teosinte_Chr1.txt")

Teosinte_Chr2 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("2")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Teosinte_Chr2.txt")

Teosinte_Chr3 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("3")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Teosinte_Chr3.txt")

Teosinte_Chr4 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("4")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Teosinte_Chr4.txt")

Teosinte_Chr5 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("5")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Teosinte_Chr5.txt")

Teosinte_Chr6 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("6")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Teosinte_Chr6.txt")

Teosinte_Chr7 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("7")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Teosinte_Chr7.txt")

Teosinte_Chr8 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("8")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Teosinte_Chr8.txt")

Teosinte_Chr9 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("9")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Teosinte_Chr9.txt")

Teosinte_Chr10 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("10")) %>%
  arrange(as.numeric(Position)) %>%
  write_csv("Teosinte_Chr10.txt")

# Subsetting maize file based on individual chr 1-10 & arranging in decreasing order of position
Teosinte_dec_Chr1 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("1")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Teosinte_dec_Chr1.txt")

Teosinte_dec_Chr2 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("2")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Teosinte_dec_Chr2.txt")

Teosinte_dec_Chr3 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("3")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Teosinte_dec_Chr3.txt")

Teosinte_dec_Chr4 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("4")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Teosinte_dec_Chr4.txt")

Teosinte_dec_Chr5 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("5")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Teosinte_dec_Chr5.txt")

Teosinte_dec_Chr6 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("6")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Teosinte_dec_Chr6.txt")

Teosinte_dec_Chr7 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("7")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Teosinte_dec_Chr7.txt")

Teosinte_dec_Chr8 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("8")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Teosinte_dec_Chr8.txt")

Teosinte_dec_Chr9 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("9")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Teosinte_dec_Chr9.txt")

Teosinte_dec_Chr10 <- teosinte_merge_file %>%
  filter(Chromosome %in% c("10")) %>%
  arrange(desc(as.numeric(Position))) %>%
  write_csv("Teosinte_dec_Chr10.txt")

#### Part2: Data Visualization
# Transposing Maize data file
genotypes_transpose_data <- setNames(data.frame(t(Genotypes_file[-1])), Genotypes_file[,1])
# Removing 2nd and 3rd column
genotypes_transpose_data <- genotypes_transpose_data[-c(1,2),]
#View(genotypes_transpose_data)
genotypes_merge_file <- merge(x = SNPs_position_file[ , c("Chromosome","Position")], y = genotypes_transpose_data, by.x = 0, by.y = 0, all.y = TRUE)
genotypes_merge_file <- genotypes_merge_file %>% rename("SNPs_ID" = "Row.names")
#View(genotypes_merge_file)

# Subsetting file for plotting SNPs per chromosome
File_plot_count <- count(genotypes_merge_file, Chromosome) %>%
  filter(Chromosome %in% c("1","2","3","4","5","6","7","8","9","10"))
# View(File_plot_count)
# Plotting SNPs per chromosome 
p_SNP_count_chr <- ggplot(File_plot_count, aes(x=Chromosome, y=n, fill=n)) + geom_bar(stat="identity") + theme_minimal()
p_SNP_count_chr

# Subsetting file for plotting distribution of SNPs on chromosomes
File_plot_dist <- genotypes_merge_file %>%
  select(Chromosome, Position) %>%
  filter(Chromosome %in% c("1","2","3","4","5","6","7","8","9","10")) %>%
  arrange(as.numeric(Position))
  #arrange(as.numeric(Chromosome), as.numeric(Position))
# View(File_plot_dist)
# PLotting distribution of SNPs on chromosomes
P_SNP_dist <- ggplot(File_plot_dist, aes(x=Chromosome, y=Position)) + geom_point(stat = "identity", position = "identity", size=1, color = "blue")
P_SNP_dist + theme(axis.text.y=element_blank())
# Preparing file for plotting proportion of homozygous and heterozygous SNPs
Genotypes_file_new <- Genotypes_file %>%
  mutate_if(is.character, str_replace_all, pattern = "A/A", replacement = "Homozygous") %>%
  mutate_if(is.character, str_replace_all, pattern = "T/T", replacement = "Homozygous") %>%
  mutate_if(is.character, str_replace_all, pattern = "G/G", replacement = "Homozygous") %>%
  mutate_if(is.character, str_replace_all, pattern = "C/C", replacement = "Homozygous") %>%
  mutate_if(is.character, str_replace_all, pattern = "A/T", replacement = "Heterozygous") %>%
  mutate_if(is.character, str_replace_all, pattern = "A/G", replacement = "Heterozygous") %>%
  mutate_if(is.character, str_replace_all, pattern = "A/C", replacement = "Heterozygous") %>%
  mutate_if(is.character, str_replace_all, pattern = "T/A", replacement = "Heterozygous") %>%
  mutate_if(is.character, str_replace_all, pattern = "T/G", replacement = "Heterozygous") %>%
  mutate_if(is.character, str_replace_all, pattern = "T/C", replacement = "Heterozygous") %>%
  mutate_if(is.character, str_replace_all, pattern = "G/A", replacement = "Heterozygous") %>%
  mutate_if(is.character, str_replace_all, pattern = "G/T", replacement = "Heterozygous") %>%
  mutate_if(is.character, str_replace_all, pattern = "G/C", replacement = "Heterozygous") %>%
  mutate_if(is.character, str_replace_all, pattern = "C/A", replacement = "Heterozygous") %>%
  mutate_if(is.character, str_replace_all, pattern = "C/T", replacement = "Heterozygous") %>%
  mutate_if(is.character, str_replace_all, pattern = "C/G", replacement = "Heterozygous") %>%
  mutate_if(is.character, str_replace_all, pattern = "\\?\\/?", replacement = "Missing")
#View(Genotypes_file_new)
#Subsetting SNP columns
MyColname <- colnames(Genotypes_file_new)[-c(1:3)]
Genotypes_file_new_long <- gather(Genotypes_file_new, SNP, Genotype, MyColname)
#head(Genotypes_file_new_long)
# Plotting the proportion heterozygous and homozygous SNPs with missing data
Plot_homo_het <- ggplot(data = Genotypes_file_new_long) + geom_bar(mapping = aes(x = Sample_ID, fill=Genotype))
Plot_homo_het + theme(axis.text.x = element_blank())

# Plotting the proportion heterozygous and homozygous SNPs with missing data across groups
Plot_homo_het <- ggplot(data = Genotypes_file_new_long) + geom_bar(mapping = aes(x = Group, fill=Genotype))
Plot_homo_het + theme(axis.text.x = element_blank())
