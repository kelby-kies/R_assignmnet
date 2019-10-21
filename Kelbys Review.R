Review by Kelby Kies

I ran the first few lines and it said it was unable to open the file. You might have to copy the path of the file.
Options: 
-I believe since you have the files in the same directory you can replace 'fang_et_al_genotypes.txt' with 
'./raw_files/fang_et_al_genotypes.txt'
- Or you could do it like we did in class and put the https location from their repository.

The part in file Inspection the command should always be file.info('./raw_files/fang_et_al_genotypes.txt', extra_cols = TRUE)
 

Your comments are very good! It helps me follow what your commands are meant to do.

At the point where you are save the files by chromosome, the files that are in decreasing position, the missing data ('?/?')
should be replaced with ('-/-'). I thnk there is a suggestion on the Slack to do this but you can also do:
  sort by decreasing position
  lapply(maize_merge_file, gsub, pattern = "?", replacement = "-", fixed = TRUE)
  
  
This command didnt work for me:
  File_plot_count <- count(genotypes_merge_file, Chromosome) %>%
    filter(Chromosome %in% c("1","2","3","4","5","6","7","8","9","10"))
I beleive you have to put Chromosome in quotes. It worked when I did that.



P_SNP_dist I dont think this shows the distributuon like you were trying to do. Try the geom_density instead of geom_point

Plot_homo_het. This plot looks sweet!!!!!
  
DId you make a plot of your own choice?

Overall you are doing great!! If you have any questions about my review or if there is any other advice I can give you just 
let me know. krkies@iastate.edu

