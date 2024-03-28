# Post-merge edits

# Identified by duplicate FE matches -- 

## Most errors should be fixed by data corrections in the pre-cleaning fix files, not here
## (ideally those corrections are reported to the dataset maintainers and fixed by them)

## But a few are the result of too little info right after incident
## and those are fixed here:  they need duplicate FE record removal

################################################################

# ### Template for one duplicate fe match: 
# ### Identify the right match by hand, then delete the dupe record 
# ### Don't forget to set FE resistant match to 1 in MakeData.R
# 
# initialmerge %>%
#   mutate(dupe.record = case_when(
#     feID==90096 & wapoID==10111 ~ 1,
#     TRUE ~ 0)
#   )
# 
# message("\n Deleting duplicate records for: \n")
# print(initialmerge %>% filter(dupe.record==1) %>% select(feID, wapoID))

# initialmerge <- initialmerge %>% 
#   filter(!dupe.record) %>%
#   select(-dupe.record)



# ### Template for two duplicate fe matches: 
# ### Identify the bad matches
# ### Delete the dupe records
# ### Don't forget to set FE resistant match to 1 in MakeData.R

initialmerge <- initialmerge %>%
  mutate(dupe.record = case_when(
    feID==90096 & wapoID==10111 ~ 1,
    feID==90097 & wapoID==10108 ~ 1,
    TRUE ~ 0)
  )

message("\n Deleting duplicate records for: \n")
print(initialmerge %>% filter(dupe.record==1) %>% select(feID, wapoID))

initialmerge <- initialmerge %>% 
  filter(!dupe.record) %>%
  select(-dupe.record)


# Re-check

dupe.fe <- table(initialmerge$feID)


if(any(dupe.fe>1)){
  
  names(dupe.fe[dupe.fe>1])
  sort(unique(dupe.fe))
  message("\n *** Duplicate FE IDs, #times")
  print(dupe.fe[dupe.fe>1])
  
  stop("\n\n fix_fe_dupes script did not fix duplicate FE IDs \n\n")
  
} else {
  message("\n *** No duplicate FE IDs after script **** \n\n")
  resistant <- 0
}
