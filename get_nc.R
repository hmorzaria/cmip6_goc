#' @title Get nc CMIP6 files
#' @details based on Puget Sound Code to download variables 
#' @author Hem Nalini Morzaria Luna hmorzarialuna@gmail.com
#' @date November 2020


#creating function parameters
get_nc <- function(model.name, model.var) {
  
  print(model.name)
#this.url <- paste("http://esgf-node.llnl.gov/esg-search/search?mip_era=CMIP6&realm=ocean&experiment_id=ssp245&frequency=mon&variable=tos&source_id=", model.name, sep = "")
  
  if(model.var %in% c("tos","sos")){

    this.url <- paste0("http://esgf-node.llnl.gov/esg-search/search?mip_era=CMIP6&realm=ocean&activity_id=ScenarioMIP&frequency=mon&variable=",model.var,"&source_id=", model.name)

  } else if (model.var %in% c("arag","calc","chl","intpp","no3","o2","ph","phyc","po4","si","talk")) {
    
    this.url <- paste0("http://esgf-node.llnl.gov/esg-search/search?mip_era=CMIP6&realm=ocnBgchem&activity_id=ScenarioMIP&frequency=mon&variable=",model.var,"&source_id=", model.name)
    
  }
  

print(this.url)
#read XML with search results for each variable
data.set <- xmlParse(this.url) %>% xmlToList() %>% unlist

#grep("esgf-data1.llnl.gov", data.set, value= TRUE)

#esgf-data1.llnl.gov

#output.var <- data.set[["result.doc.str.text"]]

get.names <- names(data.set) 

vect.locations <- grepl("result.doc.str.text",get.names)

output.var <- data.set[vect.locations]

data.list <- grep("[|]",output.var, value=TRUE)

if(!is_empty(data.list)){

#create data frame with new column names
data.set.cimp6 <- tibble(dataset = data.list) %>% 
  separate(col=dataset,c("file_source","data_source"),sep="[|]",remove=FALSE) %>% 
  separate(
    col=file_source,
    c(
      "mip_era",
      "experiment",
      "lab",
      "model",
      "experiment_id",
      "realisation",
      "realm",
      "variable",
      "gn",
      "ver"
    ),
    sep = "\\.",
    remove = FALSE
  )

#data.set.cimp6 %>% View()

for(eachrow in 1:nrow(data.set.cimp6)){
  
  this.data <- data.set.cimp6[eachrow,]
  
  this.url.wget <- paste("https://esgf-node.llnl.gov/esg-search/wget/?distrib=false&dataset_id=",this.data$dataset,sep="")
  
  this.filename <- paste(this.data$model,this.data$experiment,this.data$variable,".sh",sep="") %>% 
    gsub("-","",.) 
  
  withRestarts(
    tryCatch(
      download.file(
        url=this.url.wget,
        destfile=this.filename,
        method = "wget",
        quiet = FALSE,
        mode = "w",
        cacheOK = TRUE,
        extra = getOption("download.file.extra")
      ),
      finally = print(paste("DONE", this.filename, sep = "  "))
    ),
    abort = function() {
    },
    error = function(e) {
      print("error")
    }
  )
  
  system(paste("sudo chmod +x", this.filename))
  try(system(paste("sudo ./", this.filename, " -H https://esgf-data.dkrz.de/esgf-idp/openid/hmorzaria", sep = ""), wait=TRUE))
  #If you get asked for password Juncus#7
  
  nc.files <- list.files(pattern = "*.nc", recursive = FALSE)
  
}

}

}
