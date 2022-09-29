###################### Required ##########################
# install.packages("readxl")
library (readxl)
#############################################################
# Clear the Console and the Environment
rm(list=ls()) 

setwd("D:/ArunV/R Programming Ebird")

# xlsx files
df <- read_excel("KL_Redlist_Sample.xlsx")
colnames(df) <- c('Sl No.','Species','eBird Name','BR','NBR','WM','SM','PM','V','H','P','N','CATEGORY')

var_1 = c("Regular","Sporadic","Recent","Uncertain")

Visting_Only_Func <- function() 
{
  ifelse (df$'WM' %in% var_1 & !is.na(df$'WM')| 
            df$'SM' %in% var_1 & !is.na(df$'SM')| 
            df$'PM' %in% var_1 & !is.na(df$'PM'),
          
          ifelse (df$'WM' %in% ("Recent")| 
                    df$'SM' %in% ("Recent")|
                    df$'PM' %in% ("Recent"),
                  
                  'NA',
                  
                  ifelse (!(df$'H' %in% ('X') | df$'P' %in% ('X') | df$'N' %in% ('X')),
                          'VISTING ONLY',
                          'NA'
                  )
          ),
          'NA'
  )
}

df$CATEGORY <- 
  ifelse (df$'V' %in% ('X') & !is.na(df$'V'),
    'NA',
    ifelse (df$'BR' %in% var_1 & !is.na(df$'BR') | df$'NBR' %in% var_1 & !is.na(df$'NBR'), 
     ifelse (df$'BR' %in% ("Recent")| 
             df$'BR' %in% ("Sporadic"),
             
             Visting_Only_Func(),
             
             ifelse (!(df$'H' %in% ('X') | df$'P' %in% ('X') | df$'N' %in% ('X')),
                     
               ifelse (df$'WM' %in% var_1 & !is.na(df$'WM')| 
                         df$'SM' %in% var_1 & !is.na(df$'SM')| 
                         df$'PM' %in% var_1 & !is.na(df$'PM'),
                       
                   ifelse (df$'WM' %in% ("Recent")| 
                             df$'SM' %in% ("Recent")|
                             df$'PM' %in% ("Recent"),
                                   
                           'BREEDING ONLY',
                           
                           ifelse (!(df$'H' %in% ('X') | df$'P' %in% ('X') | df$'N' %in% ('X')),
                                   'BREEDING & VISITING',
                                   'BREEDING ONLY'
                                  )
                          ),
                  'BREEDING ONLY'),
               Visting_Only_Func())),
     Visting_Only_Func()))


# Save the dataframe in RDS file
saveRDS(df,"KL_FlowChart_Output.RDS")

# Write to csv file
write.csv(df,"D:/ArunV/R Programming Ebird/KL_FlowChart_Output.csv", row.names = FALSE)





