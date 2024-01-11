############################
## hm_corr_dendogram_info ##
############################

## hm = ComplexHeatmap pheatmap object
## names_of_groups = names of the groups made with the dendograp (character vector)
## n_objects = number of objects you have grouped. Ej if 50 samples set 50
## expression_matrix = matrix from which the data comes from, the expression matrix, NOT THE CORRELATION MATRIX !!!!!

hm_corr_dendogram_info <- function(hm, names_of_groups, 
                                   n_objects, expression_matrix){
  col_dend <- column_dend(hm)
  col_list <- column_order(hm)
  names(col_list) <- names_of_groups
  for (i in 1:length(column_order(hm))){
    print(paste(names(col_list)[i], " has ", length(column_order(hm)[[i]]), "patients"))}
  patients_data <- as.data.frame(tibble(
    patient_number = unlist(col_dend),
    cluster = rep("Not done", times = n_objects)))
  
  patients_data$patient <- "Not done"
  for (i in 1:length(rownames(patients_data))){
    patients_data$patient[i] <- colnames(expression_matrix)[patients_data$patient_number[i]]}
  
  col_list_dat <- plyr::ldply (col_list, data.frame)
  colnames(col_list_dat)[1] <- "group"
  colnames(col_list_dat)[2] <- "patient"
  
  for (i in 1:length(rownames(patients_data))){
    patients_data$cluster[i] <- col_list_dat$group[i][col_list_dat$patient == patients_data$patient_number]}
  
  patients_data <- patients_data[,c(2,3)]
  
  return(patients_data)
}            
