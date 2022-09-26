#' Obtain the factor loading matrices in the original study and delete Na value
#'
#' The data containing xlsx files(or txt files,or csv files)in the given
#' path will be extracted by this function and imported into R.These data files are
#' stored as a list in which the matrices in each file is stored in order(the order in
#' which the files are displayed in the folder).In this step,the missing values
#' of factor loading matrices from the original study stored in multiple xlsx files
#'  will be set the value of 0.
#'
#' @param type File type to extract
#' @param manual The way to read the data.
#' @usage coefa_read(type=c("xlsx","txt","csv"),manual=FALSE)
#' @details This function is mainly used to read xlsx files from a folder into R.
#' In this process,the missing values in the xlsx files(or txt file,or csv files)
#' will be set to 0.The options of the "type" parameter can be "xlsx", "txt",
#' "csv".It should be noted that the folder containing data files should be in
#' current workspace.If it is not in the current workspace, the setwd() function
#' needs to be used to set its path.An important point is that all files in your
#' folder should use the same format, for instance, all of them are the xlsx
#' files(or txt files,or csv files).
#'
#' The default "manual" parameter of this function is FALSE.When the "manual" is
#' TRUE, the function will not automatically read files. you will need to choose
#' other ways to manually input the data and put them in a list to ensure that
#' the subsequent functions of 'coefa' can work. You can get more information
#' about functions in the readme and vignettes in this package.
#' @return a list containing all factor loading matrices without NA.
#' @export
#' @examples
#' #The following only shows a case of manual reading.
#' #we recommend coefa_read(type="xlsx",manual=FALSE).
#' #You can get more information about functions in the readme and vignettes of this package.
#' coefa_read(manual=TRUE)
coefa_read<-function(type=c("xlsx","txt","csv"),manual=FALSE){
  #When the files in the folder are xlsx files.
  if(manual==FALSE){
  if(type=="xlsx"){
    listname<-dir("./",pattern = ".xlsx")
    re<-map(listname,~read.xlsx(.,))
    for (i in 1:length(re)) {
      re[[i]][is.na(re[[i]])]<-0
    }
    re1<-re
    names(re1)<-listname
    return(re1)
  }
  #When the files in the folder are txt files.
  else if(type=="txt"){
    listname<-dir("./",pattern = ".txt")
    re<-map(listname,~read.table(.,header = TRUE,sep = ","))
    for (i in 1:length(re1)) {
      re[[i]][is.na(re[[i]])]<-0
    }
    re1<-re
    names(re1)<-listname
    return(re1)
  }
  #When the files in the folder are csv files
  else if(type=="csv"){
    listname<-dir("./",pattern = ".csv")
    re<-map(listname,~read.csv(.,header = TRUE,sep = ","))
    for (i in 1:length(re)) {
      re[[i]][is.na(re[[i]])]<-0
    }
    re1<-re
    names(re1)<-listname
    return(re1)
  }}
  else if(manual==TRUE){
    message("  The function will not automatically read files from the folder and
input into R, you will need to choose other ways to manually input the data and
put them in a list to ensure that the subsequent functions of 'coefa' will
work. It should be noted that the order of studies should be consistent with
the order of sample sizes.")}
}
