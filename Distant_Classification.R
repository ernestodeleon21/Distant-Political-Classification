


############ Italy ###############

library(tidyverse)
library(stringr)

setwd('C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD IKMB/Distant Political News Classification/')

it = read.csv('Damian Data/relevant_urls/it.csv', encoding = "UTF-8")


sec = read.csv2('section_coding/Coded/it_sections.csv')


sec = sec %>% mutate('sections' = paste0('/',sections,'/'))
#Sorting sections
pol = sec %>% filter(code==1) %>% pull(sections)
eco = sec %>% filter(code==2) %>% pull(sections)
cul = sec %>% filter(code==3) %>% pull(sections)
spt = sec %>% filter(code==4) %>% pull(sections)
ent = sec %>% filter(code==5) %>% pull(sections)
crm = sec %>% filter(code==6) %>% pull(sections)
scs = sec %>% filter(code==7) %>% pull(sections)
other = sec %>% filter(code==8) %>% pull(sections)
unclear = sec %>% filter(code==9) %>% pull(sections)


#matching sections to URLs
it = it %>% mutate(sect = case_when(
  grepl(paste(pol,collapse="|"), clean_url) ~ 1,
  grepl(paste(eco,collapse="|"), clean_url) ~ 2,
  grepl(paste(cul,collapse="|"), clean_url) ~ 3,
  grepl(paste(spt,collapse="|"), clean_url) ~ 4,
  grepl(paste(ent,collapse="|"), clean_url) ~ 5,
  grepl(paste(crm,collapse="|"), clean_url) ~ 6,
  grepl(paste(scs,collapse="|"), clean_url) ~ 7,
  grepl(paste(other,collapse="|"), clean_url) ~ 8,
  TRUE ~ 9))


table(it$sect)

it = it %>% mutate(politics = ifelse(sect==1, 1, #PICK THE RIGHT SECTION CODE
                                     ifelse(sect==9,9,0)))

table(it$politics)

it = it %>% mutate(title_blurb = paste(share_title, share_main_blurb))

########## Training models Italy##########################


library(quanteda.textmodels)
library(quanteda)

it = it %>% mutate(doc_id = row_number())

corpus <- corpus(it, docid_field = "doc_id", text_field = "title_blurb")

summary(corpus) %>%
  head()

italy = dfm(corpus, remove = stopwords("italian"), 
    stem = TRUE,
    remove_punct = TRUE)

#Specify sections labeled as 'unclear' (not political nor non-political)
removed <- dfm_subset(italy, sect == 9)


head(docvars(italy))

#Remove unclear ones
italy <- dfm_subset(italy, !url_rid %in% removed$url_rid)




ndoc(italy)
ndoc(removed)


set.seed(300)
id_train <- sample(1:ndoc(italy), (ndoc(italy) * 0.75), replace = FALSE)
head(id_train, 10)

# create docvar with ID
italy$id_numeric <- 1:ndoc(italy)

# get training set
dfmat_training <- dfm_subset(italy, id_numeric %in% id_train)


# get test set (documents not in id_train)
dfmat_test <- dfm_subset(italy, !id_numeric %in% id_train)

#training model
tmod_nb <- textmodel_nb(dfmat_training, dfmat_training$politics)
summary(tmod_nb)

#testing model
dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))

actual_class <- dfmat_matched$politics
predicted_class <- predict(tmod_nb, newdata = dfmat_matched)
tab_class <- table(predicted_class, actual_class)

tab_class


library(caret)

#Testing and saving results
confusionMatrix(tab_class, mode = "everything",positive="1")
first_test = confusionMatrix(tab_class, mode = "everything",positive="1")

tocsv <- data.frame(cbind(t(first_test$overall),t(first_test$byClass)))
tocsv$model= "Italy"
tocsv$Topic = "Politics"
tocsv$Test_Type = "Train/Test Split"



table(predicted_class)


#Plotting
library(quanteda.textplots)
library(quanteda.textstats)

dfmat_training2 = dfm_group(dfmat_training, groups = politics)

tstat <- textstat_keyness(dfmat_training2)
textplot_keyness(tstat)




#Applying to rest of articles
predicted_class <- predict(tmod_nb, newdata = removed)

removed$politics <- predicted_class

table(removed$politics)

removed_test = docvars(removed)




#Testing on the handcoded data italy #############


table(it$CATEGORY)

colnames(removed_test)

removed_test2 = removed_test %>% dplyr::select(url_rid,politics)

#Adding the ones used for classification
removed_test3 = docvars(italy)
removed_test3 = removed_test3 %>% dplyr::select(url_rid,politics)

removed_test2 = rbind(removed_test2, removed_test3)

#loading and cleaning
it.coded = read.csv('Damian Data/Annotated_data/articles_annotated_it.csv')
it.coded = it.coded %>% filter(FILTER==1 | !is.na(TOPIC))
it.coded = it.coded %>% mutate(politics_coded = ifelse(TOPIC==1, 1,0)) #PICK THE RIGHT TOPIC

#Joining
it.coded = left_join(it.coded, removed_test2, by ='url_rid')

it.coded$politics_coded = factor(it.coded$politics_coded)
it.coded = it.coded %>% filter(!is.na(politics_coded))


it.coded$politics= factor(it.coded$politics)

it.coded = it.coded %>% filter(!is.na(politics))

set.seed(300)

validation = confusionMatrix(it.coded$politics, it.coded$politics_coded, mode = "everything", positive="1")
validation

tocsv <- data.frame(cbind(t(validation$overall),t(validation$byClass)))
tocsv$model= "Italy"
tocsv$Topic = "Politics"
tocsv$Test_Type = "Validation"


#Testing and saving results
confusionMatrix(tab_class, mode = "everything",positive="1")
first_test = confusionMatrix(tab_class, mode = "everything",positive="1")

tocsv2 <- data.frame(cbind(t(first_test$overall),t(first_test$byClass)))
tocsv2$model= "Italy"
tocsv2$Topic = "Politics"
tocsv2$Test_Type = "Train/Test Split"

results = rbind(tocsv,tocsv2)
