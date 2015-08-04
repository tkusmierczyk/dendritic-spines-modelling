# Data loading functions.
#
# author: tomasz.kusmierczyk(at)gmail.com
#
# Required global constants: spine.id.field.separator, spine.id.fields



ParseIdString = function(id) {
  fields = strsplit(id, spine.id.field.separator)[[1]]
  fields = as.matrix(fields)
  if (dim(fields)[1] == length(spine.id.fields)) {
    rownames(fields) = spine.id.fields
  } else if (dim(fields)[1] == length(spine.id.fields)+1) {
    rownames(fields) = c(spine.id.fields, time.id)
  } else {
    stop(paste("[ParseIdString] Failure while parsing",id))
  }
  return (fields)
}


ParseVectorOfIdStrings = function(ids) {
  result = c()
  for (id in ids) {
    row = ParseIdString(id)
    result = cbind(result, row)
  }
  return (t(result))
}


ReadInputFile = function(path) {
  # Reads single data file (must have column of name spine.id).
 
  x = read.table(path, header=T, check.names=FALSE)
  row.names(x) = x[ , spine.id]
  
  # Extracts additional fields out of id
  #for (id in x[ , spine.id]) {
  #  fields = ParseIdString(id)
  #  for (row in rownames(fields)) {      
  #    x[id, row] = fields[row, ]
  #  }
  #}  
  
  return(x)
}


# Constant time column name.
time.id = "time_id"
# Possible time column values:
time.t0 = "t0"
time.t1 = "t1"


PopulateDataAccordingToTimeMoments = function(x, 
                                              features.names.t0, 
                                              features.names.t1, 
                                              additional.features.names, 
                                              additional.features.names.t0,
                                              additional.features.names.t1,
                                              spine.id.field.separator) {
  # Copies every row twice: once with features.names.t0 and once with features.names.t1.
  #
  # Additional colum time.id is added to distinguish both groups.
  # Original id is preserved in column spine.id but rownames are updated with timestamp as the last field.
  
  additional.features.names     = intersect(additional.features.names, colnames(x))
  if (!missing(additional.features.names.t0))
    additional.features.names.t0  = intersect(additional.features.names.t0, colnames(x))  
  if (!missing(additional.features.names.t1))
    additional.features.names.t1  = intersect(additional.features.names.t1, colnames(x))
  
  x0 = x[ , c(additional.features.names, features.names.t0, additional.features.names.t0)]    
  x0[ , time.id] = time.t0
  rownames(x0) = paste(rownames(x0), time.t0, sep=spine.id.field.separator)
  
  x1 = x[ , c(additional.features.names, features.names.t1, additional.features.names.t1)]
  x1[ , time.id] = time.t1
  rownames(x1) = paste(rownames(x1), time.t1, sep=spine.id.field.separator)
  
  colnames(x1) = colnames(x0)  
  x01 = rbind(x0, x1)  
  return (x01)
}


ElementsMatchingColumnValue = function(x, col.name, col.values) {
  return (is.element(x[ , col.name], col.values)) 
}


KeepSubsetAccordingToColumnValue = function(x, col.name, col.values) {
  # Returns subset of x rows where x[,col.name] in col.values.
  return (x[ElementsMatchingColumnValue(x, col.name, col.values), ])
}



ChangeColumnNames = function(x, old.names, new.names) {
  for (no in 1:length(old.names)) {
    old.name = old.names[no]
    new.name = new.names[no]
    colnames(x)[which(colnames(x)==old.name)] = new.name
  }
  return (x)
}


DataLoadingRoutine = function(path, groups.ids, features.names.t0, features.names.t1, features.names.nice, 
                              additional.features.names, additional.features.names.t0, additional.features.names.t1,
                              spine.id.field.separator, group.column='group_id') {
  x = ReadInputFile(path)
  x = KeepSubsetAccordingToColumnValue(x, group.column, groups.ids)
  x = PopulateDataAccordingToTimeMoments(x, features.names.t0, features.names.t1, 
                                         additional.features.names,  
                                         additional.features.names.t0,  
                                         additional.features.names.t1,
                                         spine.id.field.separator)
  x = ChangeColumnNames(x, features.names.t0, features.names.nice)
  return (x)
}


