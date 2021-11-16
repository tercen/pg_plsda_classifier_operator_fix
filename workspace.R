library(tercen)
library(tercenApi)
library(dplyr, warn.conflicts = FALSE)
library(jsonlite)

library(tim)

# CONST definitions
# FULL
#http://localhost:5402/admin/w/8f58b953d254dd1a1cf982f03e011ced/ds/f563141d-149c-4318-8b19-2b6b152fb650
# SHORT
#http://localhost:5402/admin/w/1fa8ac2c09211bd45071e060ee0791f6/ds/9e862a38-d54e-4fec-b872-9777eb588fa0
options("tercen.workflowId" = "8f58b953d254dd1a1cf982f03e011ced")
options("tercen.stepId"     = "f563141d-149c-4318-8b19-2b6b152fb650")
# options("tercen.stepId"     = "9e862a38-d54e-4fec-b872-9777eb588fa0")
MCR_PATH <- "/home/rstudio/mcr/v99"
MATCALL  <- "/home/rstudio/plsda_exe/run_plsda.sh"
# =============================================



get_operator_props <- function(ctx, imagesFolder){
  MaxComponents <- -1
  Permutations   <- -1
  
  AutoScale <- "yes"
  Bagging <- "Bootstrap"
  NumberOfBags <- 24
  CrossValidation <- "LOOCV"
  Optimization<- "auto"
  QuantitationType <- "median"
  
  
  operatorProps <- ctx$query$operatorSettings$operatorRef$propertyValues
  
  for( prop in operatorProps ){
    if (prop$name == "MaxComponents"){
      MaxComponents <- as.numeric(prop$value)
    }
    
    if (prop$name == "Permutations"){
      Permutations <- as.numeric(prop$value)
    }
  }
  
  if( is.null(MaxComponents) || MaxComponents == -1 ){
    MaxComponents <- 2
  }
  
  if( is.null(Permutations) || Permutations == -1 ){
    Permutations <- 5
  }
  
  
  props <- list()
  
  props$MaxComponents <- MaxComponents
  props$Permutations <- Permutations
  
  props$AutoScale <- AutoScale
  props$Bagging <- Bagging
  props$NumberOfBags <- NumberOfBags
  props$CrossValidation <- CrossValidation
  props$Optimization<- Optimization
  props$QuantitationType <- QuantitationType
  
  
  return (props)
}




classify <- function(df, props, arrayColumns, rowColumns, colorColumns){
  outfileVis <- tempfile(fileext = ".mat")
  outfileDat <- tempfile(fileext = ".txt")
  
  
  dfJson = list(list(
    "MaxComponents"=props$MaxComponents, 
    "Permutations"=props$Permutations,
    "AutoScale"=props$AutoScale,
    "Bagging"=props$Bagging,
    "NumberOfBags"=props$NumberOfBags,
    "CrossValidation"=props$CrossValidation,
    "Optimization"=props$Optimization,
    "QuantitationType"=props$QuantitationType,
    "RowFactor"=rowColumns[[1]],
    "ColFactor"=arrayColumns[[1]],
    "OutputFileVis"=outfileVis, 
    "OutputFileDat"=outfileDat )
  )
  
  
  for( arrayCol in arrayColumns ){
    dfJson <- append( dfJson, 
                      list(list(
                        "name"=arrayCol,
                        "type"="Array",
                        "data"=pull(df, arrayCol)
                      ))
    )
  }
  for( rowCol in rowColumns ){
    dfJson <- append( dfJson,
                      list(list(
                        "name"=rowCol,
                        "type"="Spot",
                        "data"=pull(df, rowCol)
                      ))
    )
  }

  for( colorCol in colorColumns ){
    dfJson <- append( dfJson,
                      list(list(
                        "name"=colorCol,
                        "type"="color",
                        "data"=pull(df, colorCol)
                        ))
    )
  }

  dfJson <- append( dfJson,
                    list(list(
                      "name"="LFC",
                      "type"="value",
                      "data"=pull(df, ".y")
                    ) ))
  
  
  
  jsonData <- toJSON(dfJson, pretty=TRUE, auto_unbox = TRUE, digits=20)
  
  jsonFile <- "/home/rstudio/projects/pg_plsda_classifier_operator/input.json" #tempfile(fileext = ".json")
  write(jsonData, jsonFile)

  system2(MATCALL,
          args=c(MCR_PATH, " \"--infile=", jsonFile[1], "\""))
# 
  outDf <- as.data.frame( read.csv(outfileDat) )
  outDf <- outDf %>%
    rename(.ci = colSeq) %>%
    rename(.ri = rowSeq) #%>%
    #mutate(".ci"=df$.ci, .before=1) #%>%
#     #mutate(".ri"=df$.ri, .before=2) 

  # outDf <- data.frame(".ci"=0)
  
  # classifierJsonDf <- fromJSON( txt = readChar(outfileVis, file.info(outfileVis)$size)  )

  classifierModel <- readBin(outfileVis, "raw", 10e6)
  
  
  
  outDf2 <- data.frame(
    model = "model1",
    .base64.serialized.r.model = c(tim::serialise_to_string(classifierModel))
  )
  
  
  # res <- tim::get_serialized_result(
  #   df = outDf, object = classifierJsonDf, object_name = "classifierJsonDf", ctx = ctx
  # )

  
  # Cleanup
  unlink(outfileVis)
  unlink(outfileDat)
  unlink(jsonFile)
  
  return( list(outDf, outDf2) )
}



# =====================
# MAIN OPERATOR CODE
# =====================
ctx = tercenCtx()

# if (!any(ctx$cnames == "documentId")) stop("Column factor documentId is required") 


colNames  <- ctx$cnames
rowNames  <- ctx$rnames
colorCols <- ctx$colors




df <- ctx$select(c(".ci", ".ri", ".y", colorCols))



cTable <- ctx$cselect()
rTable <- ctx$rselect()

names.with.dot <- names(cTable)
names.without.dot <- names.with.dot


for( i in seq_along(names.with.dot) ){
  names.without.dot[i] <- gsub("\\.", "_", names.with.dot[i])
  colNames[i] <- gsub("\\.", "_", colNames[i])
}


names(cTable) <- names.without.dot

cTable[[".ci"]] = seq(0, nrow(cTable) - 1)
rTable[[".ri"]] = seq(0, nrow(rTable) - 1)


df = dplyr::left_join(df, cTable, by = ".ci")
df = dplyr::left_join(df, rTable, by = ".ri") 


 
props     <- get_operator_props(ctx, imgInfo[1])


tableList <- df %>%
  classify(props, unlist(colNames), unlist(rowNames), unlist(colorCols) ) 

tbl1 <- tableList[[1]]
tbl2 <- tableList[[2]]


# tbl1 = data.frame(CellLine = c("0","1","2"), Values=c(0.0,1.0,2.0))
# tbl1 %>% as_relation()
# tbl1 %>% as_relation() %>% as_join_operator(c('CellLine'), c('CellLine'))

join1 = tbl1 %>% 
  as_relation() %>%
  left_join_relation(ctx$crelation, ".ci", ctx$crelation$rids) %>%
  as_join_operator(ctx$cnames, ctx$cnames)

join2 = tbl2 %>% 
  ctx$addNamespace() %>%
  as_relation() %>%
  as_join_operator(list(), list())

join2 %>%  
  save_relation(ctx)

#%>%
#  ctx$save()

