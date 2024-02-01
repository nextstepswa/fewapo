# Post-merge edits

# Identified by duplicate matches -- 

## Most errors should be fixed by data corrections in the pre-cleaning fix files, not here
## (ideally those corrections are reported to the dataset maintainers and fixed by them)

## But a few bad matches are the result of too little info right after incident
## and those are fixed here:  they need manual unmatching and/or info reassignment

################################################################

  
# Indices for wapo vars in initialmerge
indices <- data.frame(colnames(initialmerge))
start <- as.numeric(row.names(indices)[indices=="wapoID"])
end <- ncol(initialmerge)
len <- end-start+1

# Remove wapo info from badmatch id -- needs to be selected and changed each time

badmatchid <- <wapoID>
message(paste("Fixing duplicate match for", badmatchid, "by script"))

initialmerge[initialmerge$feID==badmatchid,start:end] <- rep(NA, len) 

# Re-check cases if spotfixes have been made

dupe.wapo <- table(initialmerge$wapoID)

if(any(dupe.wapo>1)){
  names(dupe.wapo[dupe.wapo>1])
  sort(unique(dupe.wapo))
  message("\n *** Duplicate WaPo IDs, #times \n\n")
  print(dupe.wapo[dupe.wapo>1])
  
  stop("\n\n Duplicate WaPo IDs not fixed by script \n\n")

} else {
  message("\n *** No duplicate WaPo IDs after script **** \n\n")
  resistant <- 0
}


