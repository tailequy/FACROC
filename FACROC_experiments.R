library(clusterConfusion)
library(ggplot2)
source("FACROC.R")

facroc_experiment <- function(dataset=NULL, clustering_result=NULL, figure_out = NULL, 
                              protected_attr = "Gender", protected_group = "F", 
                              non_protected_group = "M", protected_label = "Female",
                              non_protected_label = "Male"){
  
  data <-read.csv(file = dataset)
  clustering <- read.csv(file = clustering_result)
  fileout <- figure_out 
  
  data_f <- data[data$gender==protected_group,]
  clustering_f <- clustering[clustering$gender==protected_group,]
  clustering_f <- clustering_f[c('cluster')]
  clustering_f <- clustering_f$cluster
  evaluation_f <- aucc(clustering_f, dataset = data_f, returnRates=TRUE)
  
  data_m <- data[data$gender==non_protected_group,]
  clustering_m <- clustering[clustering$gender==non_protected_group,]
  clustering_m <- clustering_m[c('cluster')]
  clustering_m <- clustering_m$cluster
  evaluation_m <- aucc(clustering_m, dataset = data_m, returnRates=TRUE)
  
  facroc <- compute_facroc(auccResult_protected = evaluation_f, auccResult_non_protected = evaluation_m, 
                           protected_attribute = protected_attr,protected=protected_label,non_protected=non_protected_label,
                           showPlot = TRUE, filename = fileout)
  return(facroc)
  
}

facroc_student_mat <- facroc_experiment(dataset="data/studentmat.csv", clustering_result="clustering/kmean_studentmat.csv", 
                                        figure_out = "student-mat.facroc.Long.pdf", protected_attr = "Gender", protected_group = "F", 
                                        non_protected_group = "M", protected_label = "Female", non_protected_label = "Male")

facroc_student_mat

facroc_student_por <- facroc_experiment(dataset="data/student-por-encode.csv", clustering_result="clustering/kmean_studentpor.csv", 
                  figure_out = "student-por.facroc.pdf", protected_attr = "gender", protected_group = "F", 
                  non_protected_group = "M", protected_label = "Female", non_protected_label = "Male")

facroc_student_por

facroc_adult <- facroc_experiment(dataset="data/adult_clean.csv", clustering_result="clustering/kmean_adult.csv", 
                                        figure_out = "adult.facroc.pdf", protected_attr = "gender", protected_group = "Female", 
                                        non_protected_group = "Male", protected_label = "Female", non_protected_label = "Male")

facroc_adult

facroc_german <- facroc_experiment(dataset="data/german_data_credit.csv", clustering_result="clustering/kmean_germancredit.csv", 
                                  figure_out = "german.facroc.pdf", protected_attr = "sex", protected_group = "female", 
                                  non_protected_group = "male", protected_label = "Female", non_protected_label = "Male")

facroc_german



