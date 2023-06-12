#All minor custom libraries function

#multiple-libraries---------------
#Create function to load multiple libraries simultaneously
read_library <- function(...) {
  obj <- eval(substitute(alist(...)))
  #print(obj)
  return(invisible(lapply(obj, function(x)library(toString(x), character.only=TRUE))))
}


#unwanted-levels---------------------------
# Create function to delete unwanted levels in a factor variable
delete_unwanted_levels <- function(data, variable, unwanted_levels) {
  # check if the specified variable is a factor
  if (!is.factor(data[[variable]])) {
    stop("The specified variable is not a factor.")
  }
  
  # check if the unwanted levels exist in the factor levels
  if (!all(unwanted_levels %in% levels(data[[variable]]))) {
    stop("One or more of the specified unwanted levels do not exist in the factor levels.")
  }
  
  # create a logical vector indicating which rows to keep
  keep <- !data[[variable]] %in% unwanted_levels
  
  # subset the data to keep only the rows with wanted levels
  data[keep, ]
}






#remove-text-before--------------------------------
# create function to remove section of text that appears 
# before certain point from factor level names
remove_text_from_factor_levels <- function(data, variable, char) {
  # check if the specified variable is a factor
  if (!is.factor(data[[variable]])) {
    stop("The specified variable is not a factor.")
  }
  
  # split the level names at the specified character
  level_names <- strsplit(as.character(levels(data[[variable]])), char)
  
  # keep only the part of the level names after the specified character
  level_names <- sapply(level_names, `[`, 2)
  
  # update the factor levels with the modified level names
  levels(data[[variable]]) <- level_names
  
  # return the modified data
  data
}
