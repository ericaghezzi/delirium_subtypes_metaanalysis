#DELIRIUM SUBTYPES META-ANALYSIS#

#### GENERAL SET-UP ####
#load packages
library(readr)
library(tidyverse)
library(estmeansd)
library(metafor)
library(ggthemes)
library(corrplot)
library(RColorBrewer)

#set working directory
setwd("R://GitHub/delirium_subtypes_metaanalysis/")

#import data 
#*create subtype and subtype_n variables so subtypes can be identified as number/character

data <- read_csv("data.csv") %>%
  mutate(subtype = ifelse(!is.na(hyperactive), "hyper", ifelse(!is.na(hypoactive), "hypo", ifelse(!is.na(mixed), "mix", ifelse(!is.na(nosubtype), "nosub", ifelse(!is.na(combo_hypomix), "hypomix",ifelse(!is.na(combo_hypermix), "hypermix",NA))))))) %>%
  mutate(subtype_n = ifelse(!is.na(hyperactive), 1, ifelse(!is.na(hypoactive), 2, ifelse(!is.na(mixed), 3, ifelse(!is.na(nosubtype), 4, ifelse(!is.na(combo_hypomix), 5,ifelse(!is.na(combo_hypermix), 6,NA)))))))

#create subtypes df to be used to identify subtype_n later
subtypes <- data.frame(subtype = c("hyper", "hypo","mix","nosub","hypomix","hypermix"),
                       subtype_n = c(1,2,3,4,5,6))

#create id vector to use in for loops
data <- data %>%
  mutate(id = row_number())

id <- data$id

#### ADD FACTOR CATEGORISATION ####

#add in new classification
library(readxl)
factors_new_2 <- read_excel("factors_new_4.xlsx", 
                            na = "NA")

#join new factor classifications to main data set
data <- left_join(data,factors_new_2,by=c("factor","factor_category","data_type","test_cutoff"))

#create broad factor file
broad_factors <- factors_new_2 %>%
  group_by(new_broad,new_factor) %>%
  filter(row_number()==1) %>%
  select(new_broad,new_sub,new_factor)

#### DATA WRANGLING: CONTINUOUS DATA TRANSFORMATIONS ####

#transform median/mean 95%CI/mean SEM to mean/SD
for (i in id) {
  if(data[i, "average_type"] == "median" && !is.na(data[i, "average_type"])) {
    if(data[i, "range_type"] == "range" && !is.na(data[i, "range_type"])) { #s1 (min, med, max)
      # mean and sd calc
      mean_calc <- qe.mean.sd(min.val = data[[i, "range_ll"]],
                              med.val = data[[i, "average"]],
                              max.val = data[[i, "range_ul"]],
                              n = data[[i, "N_total"]])

      data[i, "average"] <- as.numeric(mean_calc["est.mean"])
      data[i, "average_type"] <- as.character("mean")
      data[i, "variance"] <- as.numeric(mean_calc["est.sd"])
      data[i, "variance_type"] <- as.character("SD")
    }
    if(data[i, "range_type"] == "IQR" && !is.na(data[i, "range_type"])) { #s2 (q1,q3,med)
      ##del mean and sd calc
      mean_calc <- qe.mean.sd(q1.val = data[[i, "range_ll"]], 
                              med.val = data[[i, "average"]], 
                              q3.val = data[[i, "range_ul"]], 
                              n = data[[i, "N_total"]])
      
      data[i, "average"] <- as.numeric(mean_calc["est.mean"])
      data[i, "average_type"] <- as.character("mean")
      data[i, "variance"] <- as.numeric(mean_calc["est.sd"])
      data[i, "variance_type"] <- as.character("SD")
    }
  }
  if(data[i, "variance_type"] == "SEM" && !is.na(data[i, "variance_type"])) { #calculate standard deviation from SEM data
    data[i, "variance"] <- data[[i, "variance"]]*sqrt(data[[i, "N_total"]])
    data[i, "variance_type"] <- as.character("SD")
  }
  if(data[i, "variance_type"] == "SE" && !is.na(data[i, "variance_type"])) { #calculate standard deviation from SEM data
    data[i, "variance"] <- data[[i, "variance"]]*sqrt(data[[i, "N_total"]])
    data[i, "variance_type"] <- as.character("SD")
  }
}

#### DATA WRANGLING: CATEGORICAL DATA TRANSFORMATIONS ####

#calculate N with condition for all categorical data (if % or N without only reported)
for (i in id) {
  if(data[i, "data_type"] == "cat" && !is.na(data[i, "data_type"])) {
    if(is.na(data[i, "N_with_condition"])) {
      if(!is.na(data[i, "percentage_with_condition"])) {
        data[i, "N_with_condition"] <- (((data[i, "percentage_with_condition"])/100) * data[i, "N_total"])
      }
      if(!is.na(data[i, "N_without_condition"])) {
        data[i, "N_with_condition"] <- (data[i, "N_total"] - data[i, "N_without_condition"])
      }
      else {
        
      }
    }
  }
}

#### CREATE SUBTYPES COMPARISON DF ####

##covert all sex reports by female to male
for (i in id) {
  if(data[i,"new_factor"]=="Female" && !is.na(data[i,"new_factor"])) {
    data[i, "N_with_condition"] <- data[i,"N_total"] - data[i,"N_with_condition"]
    data[i,"new_factor"] <- "Male"
  }
}
 
#add in function to calculate weighted sd (to go along with any weighted mean)
grand.sd   <- function(S, M, N) {sqrt(weighted.mean(S^2 + M^2, N) -
                                        weighted.mean(M, N)^2)}

#create data type (cat/cont) variable and add all of the data in together
#1 = cat
#2 = cont

#get continuous data only and rename variables
cont <- data %>%
  filter(data_type == "cont") %>%
  filter(average_type == "mean" && variance_type == "SD" && !is.na(average_type) && !is.na(variance_type)) %>% #only keep data that has been transformed to mean/sd
  mutate(mean = average,
         sd = variance,
         n = N_total,
         n_with = as.numeric(NA)) %>%
  select(covidence_id, factor,test_subscale,test_cutoff,data_type, direction, subtype, new_broad,new_sub,new_factor, n_with, mean, sd, n)%>%
  mutate(type = 2)

#get categorical data only and rename variables
cat <- data %>%
  filter(data_type == "cat") %>%
  mutate(n_with = N_with_condition,
         n = N_total,
         mean = as.numeric(NA),
         sd = as.numeric(NA)) %>%
  select(covidence_id, factor,test_subscale,test_cutoff,data_type, direction, subtype, new_broad,new_sub,new_factor, n_with, mean, sd, n) %>%
  mutate(type = 1)

#create alltypes data file with both continuous and categorical data
alltypes <- rbind(cont,cat)

#create df for each subtype comparison which includes data for factors reported for both of these subtypes
#* 1 = hypo vs hyper
#* 2 = hypo vs mix
#* 3 = hypo vs nosub
#* 4 = hyper vs mix
#* 5 = hyper vs nosub
#* 6 = mix vs nosub
#* 7 = hypo vs all
#* 8 = hyper vs all
#* 9 = mix vs all
#* 10 = nosub vs all

#hypo vs hyper
a1 <- alltypes %>%
  filter(subtype == "hypo" | subtype == "hyper") %>% #keep only the two subtypes for this comparison
  group_by(covidence_id, type, factor,test_subscale,test_cutoff) %>%
  mutate(nrow = n()) %>%
  filter(nrow >1) %>% #this ensures for each combination of the above that there is 2 rows (1 for each subtype) 
  mutate(id = cur_group_id())
s1 <- a1 %>% filter(subtype == "hypo")
s2 <- a1 %>% filter(subtype == "hyper")
a1 <- full_join(s1,s2,by=c("id", "covidence_id","factor","test_subscale","test_cutoff","new_factor","type", "data_type","direction","new_broad","new_sub")) %>%
  mutate(analysis = 1) #label the analysis (as above)

#hypo vs mix
a2 <- alltypes %>%
  filter(subtype == "hypo" | subtype == "mix") %>% #keep only the two subtypes for this comparison
  group_by(covidence_id, type, factor,test_subscale,test_cutoff) %>%
  mutate(nrow = n()) %>%
  filter(nrow >1) %>% #this ensures for each combination of the above that there is 2 rows (1 for each subtype) 
  mutate(id = cur_group_id())
s1 <- a2 %>% filter(subtype == "hypo")
s2 <- a2 %>% filter(subtype == "mix")
a2 <- full_join(s1,s2,by=c("id", "covidence_id","factor","test_subscale","test_cutoff","new_factor","type", "data_type","direction","new_broad","new_sub")) %>%
  mutate(analysis = 2)#label the analysis (as above)

#hypo vs nosub
a3 <- alltypes %>%
  filter(subtype == "hypo" | subtype == "nosub") %>% #keep only the two subtypes for this comparison
  group_by(covidence_id, type, factor,test_subscale,test_cutoff) %>%
  mutate(nrow = n()) %>%
  filter(nrow >1) %>% #this ensures for each combination of the above that there is 2 rows (1 for each subtype) 
  mutate(id = cur_group_id())
s1 <- a3 %>% filter(subtype == "hypo")
s2 <- a3 %>% filter(subtype == "nosub")
a3 <- full_join(s1,s2,by=c("id", "covidence_id","factor","test_subscale","test_cutoff","new_factor","type", "data_type","direction","new_broad","new_sub")) %>%
  mutate(analysis = 3)#label the analysis (as above)

#hyper vs mix
a4 <- alltypes %>%
  filter(subtype == "hyper" | subtype == "mix") %>% #keep only the two subtypes for this comparison
  group_by(covidence_id, type, factor,test_subscale,test_cutoff) %>%
  mutate(nrow = n()) %>%
  filter(nrow >1) %>% #this ensures for each combination of the above that there is 2 rows (1 for each subtype) 
  mutate(id = cur_group_id())
s1 <- a4 %>% filter(subtype == "hyper")
s2 <- a4 %>%filter(subtype == "mix")
a4 <- full_join(s1,s2,by=c("id", "covidence_id","factor","test_subscale","test_cutoff","new_factor","type", "data_type","direction","new_broad","new_sub")) %>%
  mutate(analysis = 4)#label the analysis (as above)

#hyper vs nosub
a5 <- alltypes %>%
  filter(subtype == "hyper" | subtype == "nosub") %>% #keep only the two subtypes for this comparison
  group_by(covidence_id, type, factor,test_subscale,test_cutoff) %>%
  mutate(nrow = n()) %>%
  filter(nrow >1) %>% #this ensures for each combination of the above that there is 2 rows (1 for each subtype) 
  mutate(id = cur_group_id())
s1 <- a5 %>% filter(subtype == "hyper")
s2 <- a5 %>%filter(subtype == "nosub")
a5 <- full_join(s1,s2,by=c("id", "covidence_id","factor","test_subscale","test_cutoff","new_factor","type", "data_type","direction","new_broad","new_sub")) %>%
  mutate(analysis = 5)#label the analysis (as above)

#mix vs nosub
a6 <- alltypes %>%
  filter(subtype == "mix" | subtype == "nosub") %>% #keep only the two subtypes for this comparison
  group_by(covidence_id, type, factor,test_subscale,test_cutoff) %>%
  mutate(nrow = n()) %>%
  filter(nrow >1) %>% #this ensures for each combination of the above that there is 2 rows (1 for each subtype) 
  mutate(id = cur_group_id())
s1 <- a6 %>% filter(subtype == "mix")
s2 <- a6 %>%filter(subtype == "nosub")
a6 <- full_join(s1,s2,by=c("id", "covidence_id","factor","test_subscale","test_cutoff","new_factor","type", "data_type","direction","new_broad","new_sub")) %>%
  mutate(analysis = 6)#label the analysis (as above)

#hypo vs all
a7 <- alltypes %>%
  mutate(sub_confirm = ifelse(subtype == "hypo",1,0)) %>% #this adds a 1 to all rows which contain the subtype of interest data
  group_by(covidence_id, type, factor,test_subscale,test_cutoff) %>%
  mutate(sub_confirm_max = max(sub_confirm)) %>% #this should now be = to 1 for all studies which report at least the subtype of interest data for that factor
  filter(sub_confirm_max == 1) %>%
  mutate(nrow = n()) %>%
  filter(nrow >1) %>% #this ensures that there are at least two rows of data for each combination (we know from above one will be the subtype of interest)
  mutate(id = cur_group_id()) %>%
  ungroup()
s1 <- a7 %>% filter(subtype == "hypo") %>%
  select(-c(sub_confirm, sub_confirm_max))
#average across all other subtypes
s2_cat <- a7 %>% filter(subtype != "hypo") %>% #isolate all comparison data which is not the subtype of interest
  filter(data_type == "cat") %>%
  group_by(covidence_id, type, factor,test_subscale,test_cutoff) %>%
  mutate(n_with = sum(n_with), #add all together to get pooled estimate
         n = sum(n), #add all together to get pooled estimate
         mean = as.numeric(NA),
         sd = as.numeric(NA)) %>% 
  filter(row_number()==1) %>%
  ungroup()
s2_cont <- a7 %>% filter(subtype != "hypo") %>% #isolate all comparison data which is not the subtype of interest
  filter(data_type == "cont") %>%
  group_by(covidence_id, type, factor,test_subscale,test_cutoff) %>%
  mutate(mean = weighted.mean(mean, n),
         sd = grand.sd(sd, mean, n),
         n = sum(n), #add all together to get pooled estimate
         n_with = as.numeric(NA)) %>% 
  filter(row_number()==1) %>%
  ungroup()
  
s2 <- rbind(s2_cat,s2_cont) %>%
  select(-c(sub_confirm, sub_confirm_max)) %>%
  mutate(subtype = "not hypo")

a7 <- full_join(s1,s2,by=c("id", "covidence_id","factor","test_subscale","test_cutoff","new_factor","type", "data_type","direction","new_broad","new_sub")) %>%
  mutate(analysis = 7)#label the analysis (as above)

#hyper vs all
a8 <- alltypes %>%
  mutate(sub_confirm = ifelse(subtype == "hyper",1,0)) %>% #this adds a 1 to all rows which contain the subtype of interest data
  group_by(covidence_id, type, factor,test_subscale,test_cutoff) %>%
  mutate(sub_confirm_max = max(sub_confirm)) %>% #this should now be = to 1 for all studies which report at least the subtype of interest data for that factor
  filter(sub_confirm_max == 1) %>%
  mutate(nrow = n()) %>%
  filter(nrow >1) %>% #this ensures that there are at least two rows of data for each combination (we know from above one will be the subtype of interest)
  mutate(id = cur_group_id()) %>%
  ungroup()
s1 <- a8 %>% filter(subtype == "hyper")%>%
  select(-c(sub_confirm, sub_confirm_max))
#average across all other subtypes
s2_cat <- a8 %>% filter(subtype != "hyper") %>% #isolate all comparison data which is not the subtype of interest
  filter(data_type == "cat") %>%
  group_by(covidence_id, type, factor,test_subscale,test_cutoff) %>%
  mutate(n_with = sum(n_with), #add all together to get pooled estimate
         n = sum(n), #add all together to get pooled estimate
         mean = as.numeric(NA),
         sd = as.numeric(NA)) %>% 
  filter(row_number()==1) %>%
  ungroup()
s2_cont <- a8 %>% filter(subtype != "hyper") %>% #isolate all comparison data which is not the subtype of interest
  filter(data_type == "cont") %>%
  group_by(covidence_id, type, factor,test_subscale,test_cutoff) %>%
  mutate(mean = weighted.mean(mean, n),
         sd = grand.sd(sd, mean, n),
         n = sum(n), #add all together to get pooled estimate
         n_with = as.numeric(NA)) %>% 
  filter(row_number()==1) %>%
  ungroup()

s2 <- rbind(s2_cat,s2_cont) %>%
  select(-c(sub_confirm, sub_confirm_max)) %>%
  mutate(subtype = "not hyper")

a8 <- full_join(s1,s2,by=c("id", "covidence_id","factor","test_subscale","test_cutoff","new_factor","type", "data_type","direction","new_broad","new_sub")) %>%
  mutate(analysis = 8)#label the analysis (as above)

#mix vs all
a9 <- alltypes %>%
  mutate(sub_confirm = ifelse(subtype == "mix",1,0)) %>% #this adds a 1 to all rows which contain the subtype of interest data
  group_by(covidence_id, type, factor,test_subscale,test_cutoff) %>%
  mutate(sub_confirm_max = max(sub_confirm)) %>% #this should now be = to 1 for all studies which report at least the subtype of interest data for that factor
  filter(sub_confirm_max == 1) %>%
  mutate(nrow = n()) %>%
  filter(nrow >1) %>% #this ensures that there are at least two rows of data for each combination (we know from above one will be the subtype of interest)
  mutate(id = cur_group_id()) %>%
  ungroup()
s1 <- a9 %>% filter(subtype == "mix")%>%
  select(-c(sub_confirm, sub_confirm_max))
#average across all other subtypes
s2_cat <- a9 %>% filter(subtype != "mix") %>% #isolate all comparison data which is not the subtype of interest
  filter(data_type == "cat") %>%
  group_by(covidence_id, type, factor,test_subscale,test_cutoff) %>%
  mutate(n_with = sum(n_with), #add all together to get pooled estimate
         n = sum(n), #add all together to get pooled estimate
         mean = as.numeric(NA),
         sd = as.numeric(NA)) %>% 
  filter(row_number()==1) %>%
  ungroup()
s2_cont <- a9 %>% filter(subtype != "mix") %>% #isolate all comparison data which is not the subtype of interest
  filter(data_type == "cont") %>%
  group_by(covidence_id, type, factor,test_subscale,test_cutoff) %>%
  mutate(mean = weighted.mean(mean, n),
         sd = grand.sd(sd, mean, n),
         n = sum(n), #add all together to get pooled estimate
         n_with = as.numeric(NA)) %>% 
  filter(row_number()==1) %>%
  ungroup()

s2 <- rbind(s2_cat,s2_cont) %>%
  select(-c(sub_confirm, sub_confirm_max)) %>%
  mutate(subtype = "not mix")

a9 <- full_join(s1,s2,by=c("id", "covidence_id","factor","test_subscale","test_cutoff","new_factor","type", "data_type","direction","new_broad","new_sub")) %>%
  mutate(analysis = 9)#label the analysis (as above)

#nosub vs all
a10 <- alltypes %>%
  mutate(sub_confirm = ifelse(subtype == "nosub",1,0)) %>% #this adds a 1 to all rows which contain the subtype of interest data
  group_by(covidence_id, type, factor,test_subscale,test_cutoff) %>%
  mutate(sub_confirm_max = max(sub_confirm)) %>% #this should now be = to 1 for all studies which report at least the subtype of interest data for that factor
  filter(sub_confirm_max == 1) %>%
  mutate(nrow = n()) %>%
  filter(nrow >1) %>% #this ensures that there are at least two rows of data for each combination (we know from above one will be the subtype of interest)
  mutate(id = cur_group_id()) %>%
  ungroup()
s1 <- a10 %>% filter(subtype == "nosub")%>%
  select(-c(sub_confirm, sub_confirm_max))
#average across all other subtypes
s2_cat <- a10 %>% filter(subtype != "nosub") %>% #isolate all comparison data which is not the subtype of interest
  filter(data_type == "cat") %>%
  group_by(covidence_id, type, factor,test_subscale,test_cutoff) %>%
  mutate(n_with = sum(n_with), #add all together to get pooled estimate
         n = sum(n), #add all together to get pooled estimate
         mean = as.numeric(NA),
         sd = as.numeric(NA)) %>% 
  filter(row_number()==1) %>%
  ungroup()
s2_cont <- a10 %>% filter(subtype != "nosub") %>% #isolate all comparison data which is not the subtype of interest
  filter(data_type == "cont") %>%
  group_by(covidence_id, type, factor,test_subscale,test_cutoff) %>%
  mutate(mean = weighted.mean(mean, n),
         sd = grand.sd(sd, mean, n),
         n = sum(n), #add all together to get pooled estimate
         n_with = as.numeric(NA)) %>% 
  filter(row_number()==1) %>%
  ungroup()

s2 <- rbind(s2_cat,s2_cont) %>%
  select(-c(sub_confirm, sub_confirm_max)) %>%
  mutate(subtype = "not nosub")

a10 <- full_join(s1,s2,by=c("id", "covidence_id","factor","test_subscale","test_cutoff","new_factor","type", "data_type","direction","new_broad","new_sub")) %>%
  mutate(analysis = 10)#label the analysis (as above)

#create analysis df
comparison <- rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) #this document has one row per data_type/factor/cutoff for each subtype comparison (which comparison is represented by the analysis variable)

#### CALCULATE EFFECT SIZES ####

#create id variable (id = row number)
comparison <- comparison %>%
  ungroup()%>%
  mutate(id = row_number())

#calculate effect sizes for categorical data
cat_es <- comparison %>%
  filter(type == "1")

cat_es <- escalc(measure = "OR",
              ai = n_with.x,
              bi = n.x - n_with.x,
              n1i = n.x,
              ci = n_with.y,
              di = n.y - n_with.y,
              n2i = n.y,
              data = cat_es)

#calculate effect sizes for continuous data
cont_es <- comparison %>%
  filter(type == "2")

cont_es <- escalc(measure = "SMD",
                  m1i = mean.x,
                  sd1i = sd.x,
                  n1i = n.x,
                  m2i = mean.y,
                  sd2i = sd.y,
                  n2i = n.y,
                  data = cont_es)

#change any reverse coded variables
id <- 1:nrow(cont_es)
for (i in id) {
  if (cont_es[i,"direction"]=="negative" && !is.na(cont_es[i,"direction"])) {
    cont_es[i,"yi"] <- cont_es[i,"yi"]*-1
  }
}

#create between groups df with all es
comparison_es <- rbind(cont_es,cat_es)

#average across effect sizes of multiple of the same factor in the same grouping reported for each type and subtype comparison within each study
comparison_es <- comparison_es %>%
  group_by(covidence_id,analysis,type,new_broad,new_sub,new_factor) %>%
  mutate(yi = mean(yi,na.rm =T),
         vi = mean(vi,na.rm =T),
         n_with.x = mean(n_with.x),
         n_total.x = mean(n.x),
         n_with.y = mean(n_with.y),
         n_total.y = mean(n.y)) %>%
  filter(row_number()==1)%>%
  ungroup() #%>%

#### CREATE ANALYSIS DF ####

#create comparison analysis df
#average across necessary variables to make complete analysis file (each study represented up to once per analysis)
c_analysis <- comparison_es %>%
  group_by(covidence_id, type, analysis, new_factor) %>%
  mutate(yi = mean(yi, na.rm = T),
         vi = mean(vi, na.rm = T)) %>% #obtain average g and st err within each study (average across domains), removing NAs from calculation
  filter(row_number()==1) %>% #make each study only appear on one row (per measure, timepoint, type)
  ungroup() %>%
  filter(!is.na(yi))

#### SET-UP META-ANALYSIS OUTPUT DF ####

#create df for analysis output
output <- data.frame(matrix(ncol=23,nrow=0, dimnames=list(NULL, c("analysis", "subtype_n", "factor_n", "factor","type", "subtype_comp", "k", "N1","N2", "b", "ci.lb", "ci.ub", "p","I2", "tau2", "se.tau2","QE","QEp", "eggers_int","eggers_p","trim_k0","trim_b","trim_se"))))

#assign variable types as needed
output$analysis <- as.character(output$analysis)
output$subtype_n <- as.numeric(output$subtype)
output$factor_n <- as.numeric(output$factor_n)
output$factor <- as.numeric(output$factor)
output$type <- as.numeric(output$type) #use this if run separate analyses for data type
output$subtype_comp <- as.numeric(output$subtype_comp)
output$N2 <- as.numeric(output$N2)
output$k <- as.numeric(output$k)
output$N1 <- as.numeric(output$N1)
output$b <- as.numeric(output$b)
output$ci.lb <- as.numeric(output$ci.lb)
output$ci.ub <- as.numeric(output$ci.ub)
output$p <- as.numeric(output$p)
output$I2 <- as.numeric(output$I2)
output$tau2 <- as.numeric(output$tau2)
output$se.tau2 <- as.numeric(output$se.tau2)
output$QE <- as.numeric(output$QE)
output$QEp <- as.numeric(output$QEp)
output$eggers_int <- as.numeric(output$eggers_int)
output$eggers_p <- as.numeric(output$eggers_p)
output$trim_k0 <- as.numeric(output$trim_k0)
output$trim_b <- as.numeric(output$trim_b)
output$trim_se <- as.numeric(output$trim_se)

#### META-ANALYSIS ####

#give unique n to each unique factor across all studies
factors <- c_analysis %>%
  group_by(new_factor) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  mutate(factor_n = row_number()) %>%
  select(new_factor,factor_n)

#add factor_n to comparison data
c_analysis <- left_join(c_analysis,factors, by = "new_factor")

type <- (1:2) #loop through data type (cat/cont)
analysis <- (1:10) #these are the subtype comparisons to loop through
factor <- c(1:max(factors$factor_n)) #make vector with N factors to cycle through

#run meta-analysis for each factor for each analysis (subtype comparison)
for (t in type) {
for(a in analysis) {
  for(f in factor) {
    df <- c_analysis %>%
      filter(type == t) %>%
      filter(analysis == a) %>%
      filter(factor_n == f) 
    
    if(nrow(df) > 2) { #reported in >2 studies
      ma <- rma(yi = yi, vi = vi, data = df, method = "PM", test = "knha")
      
      output <- output %>%
        add_row(analysis="comparison",
                subtype_n=NA,
                factor_n=f,
                factor=NA,
                type=t,
                subtype_comp=a,
                k=ma$k,
                N1=sum(df$n.x),
                N2=sum(df$n.y),
                b=ma$b,
                ci.lb=ma$ci.lb,
                ci.ub=ma$ci.ub,
                p=ma$pval,
                I2=ma$I2,
                tau2=ma$tau2,
                se.tau2=ma$se.tau2,
                QE=ma$QE,
                QEp=ma$QEp,
                eggers_int = NA,
                eggers_p=NA,
                trim_k0 = NA,
                trim_b = NA,
                trim_se = NA)
    }

  }
}
}

#add in factor names by matching with factor_n from factors doc
output <- left_join(output,factors,by = "factor_n") %>%
  mutate(factor = new_factor) %>%
  select(-new_factor)

#### PUBLICATION BIAS ####

id <- 1:nrow(output)

#create funnel plots
for(i in id) {
  if(output[i,"type"]==1) {
      factor_name <- output[i,"factor"]
      comp_name <- output[i,"subtype_comp"]
      funnel_name <- paste0(factor_name, "_comp_", comp_name, "_cat.tiff")

      t <- output[[i,"type"]]
      a<- output[[i,"subtype_comp"]]
      f<- output[[i,"factor_n"]]

      df <- c_analysis %>%
        filter(type == t) %>%
        filter(analysis == a) %>%
        filter(factor_n == f)

      ma <- rma(yi = yi, vi = vi, data = df, method = "PM", test = "knha")

      tiff(filename = funnel_name, width = 8, height = 6, units = "in", res = 300, compression = "lzw")
      funnel(ma, trans = exp, xlab = "OR")
      dev.off()
  }
  if(output[i,"type"]==2) {
    factor_name <- output[i,"factor"]
    comp_name <- output[i,"subtype_comp"]
    funnel_name <- paste0(factor_name, "_comp_", comp_name, "_cont.tiff")

    t <- output[[i,"type"]]
    a<- output[[i,"subtype_comp"]]
    f<- output[[i,"factor_n"]]

    df <- c_analysis %>%
      filter(type == t) %>%
      filter(analysis == a) %>%
      filter(factor_n == f)

    ma <- rma(yi = yi, vi = vi, data = df, method = "PM", test = "knha")

    tiff(filename = funnel_name, width = 8, height = 6, units = "in", res = 300, compression = "lzw")
    funnel(ma, trans = exp, xlab = "OR")
    dev.off()
  }
}

#publication bias assessment (for analyses with k>9)
for(i in id) {
      if(output[i,"k"]>9){
        t <- output[[i,"type"]]
        a<- output[[i,"subtype_comp"]]
        f<- output[[i,"factor_n"]]

        df <- c_analysis %>%
          filter(type == t) %>%
          filter(analysis == a) %>%
          filter(factor_n == f)

        ma <- rma(yi = yi, vi = vi, data = df, method = "PM", test = "knha")

        reg <- regtest(ma, model = "rma")

        output[i,"eggers_int"] <- reg$zval
        output[i,"eggers_p"]<- reg$pval

        if(output[i,"eggers_p"]<0.1){
          trim <- trimfill(ma)

          output[i,"trim_k0"] <- trim$k0
          output[i,"trim_b"] <- exp(trim$b)
          output[i,"trim_se"] <- trim$se

      }
    }
  }

#### ADD LABELS TO OUTPUT FILE ####
#add broad factor categories
broad <- broad_factors %>%
  ungroup() %>%
  mutate(factor = new_factor) %>%
  select(new_broad,new_sub,factor)

output <- left_join(output,broad,by="factor")

#add comparison group labels
subtype_comp <- c(1:10)
comp_lab <- c("hypoactive vs hyperactive","hypoactive vs mixed","hypoactive vs no motor subtype","hyperactive vs mixed","hyperactive vs no motor subtype","mixed vs no motor subtype", "hypoactive vs remaining","hyperactive vs remaining","mixed vs remaining","no motor subtype vs remaining")
comp_labs <- data.frame(subtype_comp,comp_lab)

comp_output <- left_join(output,comp_labs,by="subtype_comp")

#get unique studies which were eligible to be analysed (>2 data points per type, analysis, factor_n)
c_studies <- c_analysis %>%
  group_by(type,analysis,factor_n) %>%
  mutate(nrow = n()) %>%
  filter(nrow > 2) %>%
  ungroup() %>%
  group_by(covidence_id) %>%
  filter(row_number()==1) %>%
  select(covidence_id)

#get analyses each paper is included in
study_factors <- c_analysis %>%
  group_by(type,analysis,factor_n) %>%
  mutate(nrow = n()) %>%
  filter(nrow > 2) %>%
  group_by(covidence_id,type,new_factor,analysis) %>%
  filter(row_number()==1) 

#get all study names (author, year)
study_names <- read.csv("demo_data.csv") %>%
  select(covidence_id, author,year)

#add study names to study factors df
study_factors <- left_join(study_factors,study_names,by="covidence_id") %>%
  arrange(author,year,type,new_broad,new_factor, analysis) %>%
  select(covidence_id,author,year,type,new_broad,new_factor,analysis)

#add comparison labels
analysis <- c(1:10)
comp_lab <- c("hypo vs hyper","hypo vs mix","hypo vs nosub","hyper vs mix","hyper vs nosub","mix vs nosub", "hypo vs remaining","hyper vs remaining","mix vs remaining","nosub vs remaining")
comp_labs <- data.frame(analysis,comp_lab)

study_factors <- left_join(study_factors,comp_labs,by="analysis")

#write output file for study factors
write.csv(study_factors, file = "study_factors.csv")

#get table with remaining factors and their corresponding studies which were not included in meta-analysis
analysed_factors <- c_analysis %>%
  group_by(type,analysis,factor_n) %>%
  mutate(nrow = n()) %>%
  filter(nrow > 2) %>%
  group_by(covidence_id,type,new_factor) %>%
  filter(row_number()==1) %>%
  ungroup()

all_factors <- c_analysis %>%
  group_by(covidence_id,type,new_factor) %>%
  filter(row_number()==1) %>%
  ungroup()

excluded_factors <- anti_join(all_factors,analysed_factors,by=c("covidence_id","type","new_factor")) %>%
  arrange(new_broad,new_sub,new_factor,type,covidence_id) %>%
  select(new_broad,new_sub,new_factor,type,covidence_id)

#add on study labels
excluded_factors <- left_join(excluded_factors,study_names,by="covidence_id") %>%
  select(new_broad,new_sub,new_factor,type,author,year)

#write csv
write.csv(excluded_factors,file = "excludedfactor_study.csv")

#### GET % FOR CATEGORICAL ANALYSES ####
percent <- c_analysis %>%
  filter(type == 1) %>%
  group_by(analysis,factor_n) %>%
  mutate(nrow = n()) %>%
  filter(nrow > 2) %>% #now have all data points which were analysed
  mutate(n_with_1 = sum(n_with.x),
         n_1 = sum(n.x),
         n_with_2 = sum(n_with.y),
         n_2 = sum(n.y)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  mutate(percent.x = (n_with_1 / n_1)*100,
         percent.y = (n_with_2 / n_2)*100) %>%
  select(new_factor,new_broad,new_sub,analysis,percent.x,percent.y) %>%
  mutate(subtype_comp = analysis)%>%
  select(-analysis)%>%
  mutate(factor = new_factor) %>%
  select(-new_factor) %>%
  mutate(type =1)

#add this onto comp_output file
comp_output <- left_join(comp_output,percent,by=c("factor","new_broad","new_sub","type","subtype_comp"))

#### CREATE FIGURE 1 and 2 (CORRPLOT) ####
#get continuous data and arrange in descending order based on factor variables (so reads top - bottom on graph)
comp_output_cont <- comp_output %>%
  filter(type ==2) %>%
  arrange(desc(new_broad,new_sub,factor)) %>%
  select(factor, comp_lab, b)

#pivot wider so each row = one broad category and each column = a subtype comparison
comp_wide_cont <- comp_output_cont %>%
  pivot_wider(names_from = comp_lab, values_from = b) %>%
  select(factor,`hypoactive vs hyperactive`,`hypoactive vs mixed`,`hypoactive vs no motor subtype`,`hyperactive vs mixed`,`hyperactive vs no motor subtype`,`mixed vs no motor subtype`,`hypoactive vs remaining`,`hyperactive vs remaining`,`mixed vs remaining`,`no motor subtype vs remaining`)

#change row names to the categories (so this is recognised by corr plot)
comp_wide_cont <- as.data.frame(comp_wide_cont)
row.names(comp_wide_cont)= comp_wide_cont$factor
comp_wide_cont = comp_wide_cont[,-1]

#make the df into a matrix
comp_wide_cont <- as.matrix(comp_wide_cont)

#do same as above but get p value matrix
comp_output_cont_p <- comp_output %>%
  filter(type ==2) %>%
  arrange(desc(new_broad,new_sub,factor)) %>%
  select(factor, comp_lab, p)

comp_wide_cont_p <- comp_output_cont_p %>%
  pivot_wider(names_from = comp_lab, values_from = p)%>%
  select(factor,`hypoactive vs hyperactive`,`hypoactive vs mixed`,`hypoactive vs no motor subtype`,`hyperactive vs mixed`,`hyperactive vs no motor subtype`,`mixed vs no motor subtype`,`hypoactive vs remaining`,`hyperactive vs remaining`,`mixed vs remaining`,`no motor subtype vs remaining`)

comp_wide_cont_p <- as.data.frame(comp_wide_cont_p)
row.names(comp_wide_cont_p)= comp_wide_cont_p$factor
comp_wide_cont_p = comp_wide_cont_p[,-1]
comp_wide_cont_p <- as.matrix(comp_wide_cont_p)

#continuous corrplot
tiff(filename = "cont_corr.tiff", compression = "lzw", width = 8,height = 5, units = "in", res = 300)
corrplot(comp_wide_cont, col=brewer.pal(n=8, name="RdBu"), tl.col = "black", tl.srt = 60, na.label = "-", na.label.col = "gray49",
         p.mat = comp_wide_cont_p, insig = "pch", pch.col = "gray49", pch.cex = 2)
dev.off()

#categorical graph (as above for continuous)
comp_output_cat <- comp_output %>%
  filter(type == 1) %>%
  arrange(desc(new_broad,new_sub,factor)) %>%
  mutate(exp_or = exp(b))%>% #get exp(OR) in order to transform log odds to OR
  select(factor, comp_lab, exp_or)

comp_wide_cat <- comp_output_cat %>%
  pivot_wider(names_from = comp_lab, values_from = exp_or)%>%
  select(factor,`hypoactive vs hyperactive`,`hypoactive vs mixed`,`hypoactive vs no motor subtype`,`hyperactive vs mixed`,`hyperactive vs no motor subtype`,`mixed vs no motor subtype`,`hypoactive vs remaining`,`hyperactive vs remaining`,`mixed vs remaining`,`no motor subtype vs remaining`)

comp_wide_cat <- as.data.frame(comp_wide_cat)
row.names(comp_wide_cat)= comp_wide_cat$factor
comp_wide_cat = comp_wide_cat[,-1]

comp_wide_cat <- as.matrix(comp_wide_cat)

#get p value matrix
comp_output_cat_p <- comp_output %>%
  filter(type == 1) %>%
  arrange(desc(new_broad,new_sub,factor)) %>%
  select(factor, comp_lab, p)

comp_wide_cat_p <- comp_output_cat_p %>%
  pivot_wider(names_from = comp_lab, values_from = p)%>%
  select(factor,`hypoactive vs hyperactive`,`hypoactive vs mixed`,`hypoactive vs no motor subtype`,`hyperactive vs mixed`,`hyperactive vs no motor subtype`,`mixed vs no motor subtype`,`hypoactive vs remaining`,`hyperactive vs remaining`,`mixed vs remaining`,`no motor subtype vs remaining`)

comp_wide_cat_p <- as.data.frame(comp_wide_cat_p)
row.names(comp_wide_cat_p)= comp_wide_cat_p$factor
comp_wide_cat_p = comp_wide_cat_p[,-1]
comp_wide_cat_p <- as.matrix(comp_wide_cat_p)

#categorical corrplot
tiff(filename = "cat_corr.tiff", compression = "lzw", width = 8,height = 8, units = "in", res = 300)
corrplot(comp_wide_cat, is.corr=F,col=brewer.pal(n=8, name="RdBu"), tl.col = "black", tl.srt = 60, na.label = "-", na.label.col = "gray49",
         p.mat = comp_wide_cat_p, insig = "pch", pch.col = "gray49", pch.cex = 2,col.lim = c(0,2))
dev.off()

#### OUTPUT FILE ####
#change cat to OR (not log odds)
cat_comp_output <- comp_output %>%
  filter(type ==1) %>%
  mutate(b = exp(b),
         ci.lb=exp(ci.lb),
         ci.ub=exp(ci.ub)) 

cont_comp_output <- comp_output %>% filter(type==2)

#join back together
comp_output <- rbind(cat_comp_output,cont_comp_output)

#round all to appropriate dp
comp_output$b <- round(comp_output$b, digits = 2)
comp_output$ci.lb <- round(comp_output$ci.lb, digits = 2)
comp_output$ci.ub <- round(comp_output$ci.ub, digits = 2)
comp_output$p <- round(comp_output$p, digits = 3)
comp_output$I2 <- round(comp_output$I2, digits = 2)
comp_output$tau2 <- round(comp_output$tau2, digits = 2)

#create output doc
comp_output_tab <- comp_output %>%
  arrange(type,new_broad,new_sub,factor) %>%
  mutate(ci = paste0(ci.lb, " - ", ci.ub)) %>%
  select(type,new_broad,new_sub,factor,comp_lab,k,N1,N2,b,ci,p,I2,tau2,percent.x,percent.y,eggers_int,eggers_p,trim_k0,trim_b,trim_se)

write.csv(comp_output_tab,"output.csv")

####PUBLICATION BIAS OUTPUT FILE####
#just for significant analyses
output_bias <- comp_output %>%
  ungroup() %>%
  arrange(type,new_broad,new_sub,factor) %>%
  filter(p <0.05) %>%
  select(type,new_broad,factor,subtype_comp,comp_lab,eggers_int:trim_se)

write.csv(output_bias,file="output_bias.csv")