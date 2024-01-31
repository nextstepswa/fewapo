# Post-merge edits

# Likely to be temporary

# Identified by mismatched date+city, which can be resolved
# by inspection and fixed in the pre/post cleaning files
################################################################


# ### Indices for wapo vars in mergefull
# indices <- data.frame(colnames(mergefull))
# start <- as.numeric(row.names(indices)[indices=="wapoID"])
# end <- as.numeric(row.names(indices)[indices=="year.y"])
# len <- end-start+1
# 
# ### Template for bad matches removal
# mergefull[mergefull$feID==25615,start:end] <- rep(NA, len) # fixes wapoID 4568 (may not be a fatality, see below)
# 
# ### fix for duplicate wapoID match: delete bad match, leave match to 31223
# mergefull[mergefull$feID==31303,start:end] <- rep(NA, len)
# mergefull[mergefull$feID==31303,]$wapoID <- NA
#
# ### Template for duplicate fe match: delete dupe record 
# mergefull[mergefull$feID==90013,start:end] <- rep(NA, len) # remove WaPo matches for both
# keep <- mergefull[mergefull$feID == 90013,][1,]
# mergefull <- bind_rows(mergefull[mergefull$feID != 90013,], keep)

# Re-check cases if spotfixes have been made
# # Re-check city/date mismatch
# mergefull %>%
#     filter(city.y != city.x & date.x != date.y) %>%
#     select(feID, wapoID, city.x, city.y, date.x, date.y)

# spotfix WA homicide designations: optional 
## we don't currently do this b/c 
## we haven't looked at all cases, all years, so don't want to make
## one anomalous fix
# mutate(homicide = ifelse(feID == 28698 | #deputy crash while having stroke
#                              feID == 25804 | #next 2 killed by suspect Tad Norman, not police
#                              feID == 25805, 
#                          0, homicide))
