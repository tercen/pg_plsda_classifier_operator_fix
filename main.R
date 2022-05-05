library(tercen)
library(tercenApi)
library(dplyr, warn.conflicts = FALSE)
library(jsonlite)

library(tim)

MCR_PATH <- "/opt/mcr/v99"
MATCALL  <- "/mcr/exe/run_plsda.sh"
# =============================================

# http://127.0.0.1:5402/test-team/w/bddd3c84d46cb31435843a71c207aff5/ds/6741c225-bbc6-4ecb-ab04-8390c856d0c0
#options("tercen.workflowId" = "bddd3c84d46cb31435843a71c207aff5")
#options("tercen.stepId"     = "6741c225-bbc6-4ecb-ab04-8390c856d0c0")

# http://127.0.0.1:5402/admin/w/64c13d425852ec95487a08924a0025d0/ds/ecc4629b-0165-4e1d-86a5-036e845f1db5
#options("tercen.workflowId" = "64c13d425852ec95487a08924a0025d0")
#options("tercen.stepId"     = "ecc4629b-0165-4e1d-86a5-036e845f1db5")

#ADULT
#http://127.0.0.1:5402/admin/w/64c13d425852ec95487a08924a0025d0/ds/45390a4e-9d46-40bb-8c6f-8121944ac451
#options("tercen.workflowId" = "64c13d425852ec95487a08924a0025d0")
#options("tercen.stepId"     = "45390a4e-9d46-40bb-8c6f-8121944ac451")


get_operator_props <- function(ctx, imagesFolder){
  MaxComponents <- -1
  Permutations   <- -1
  
  AutoScale <- "yes"
  Bagging <- "Bootstrap"
  NumberOfBags <- -1
  CrossValidation <- "LOOCV"
  Optimization<- "auto"
  QuantitationType <- "median"
  DiagnosticPlot <- "Advanced"
  
  
  operatorProps <- ctx$query$operatorSettings$operatorRef$propertyValues
  
  for( prop in operatorProps ){
    if (prop$name == "MaxComponents"){
      MaxComponents <- as.numeric(prop$value)
    }
    
    if (prop$name == "AutoScale"){
      AutoScale <- prop$value
    }
    
    if (prop$name == "Bagging"){
      Bagging <- prop$value
    }
    
    if (prop$name == "NumberOfBags"){
      NumberOfBags <- as.numeric(prop$value)
    }
    
    if (prop$name == "CrossValidation"){
      CrossValidation <- prop$value
    }
    
    if (prop$name == "Optimization"){
      Optimization <- prop$value
    }
    
    if (prop$name == "DiagnosticPlot"){
      DiagnosticPlot <- prop$value
    }
    
  }
  
  if( is.null(DiagnosticPlot) ){
    DiagnosticPlot <- "Advanced"
  }
  
  if( is.null(MaxComponents) || MaxComponents == -1 ){
    MaxComponents <- 2
  }
  
  if( is.null(Permutations) || Permutations == -1 ){
    Permutations <- 5
  }
  
  if( is.null(NumberOfBags) || NumberOfBags == -1 ){
    NumberOfBags <- 24
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
  props$DiagnosticPlot <- DiagnosticPlot
  
  
  return (props)
}




classify <- function(df, props, arrayColumns, rowColumns, colorColumns){
  baseName <- '/tmp/hhh'
  
  outfileVis <- tempfile(fileext = ".mat")
  outfileDat <- tempfile(fileext = ".txt")
  outfileImg <- tempfile(fileext = ".svg")

  
  dfJson = list(list(
    "MaxComponents"=props$MaxComponents, 
    "Permutations"=props$Permutations,
    "AutoScale"=props$AutoScale,
    "Bagging"=props$Bagging,
    "NumberOfBags"=props$NumberOfBags,
    "CrossValidation"=props$CrossValidation,
    "Optimization"=props$Optimization,
    "QuantitationType"=props$QuantitationType,
    "DiagnosticPlot"=props$DiagnosticPlot,
    "DiagnosticPlotPath"=outfileImg,
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
  
  jsonFile <- tempfile(fileext = ".json")
  #jsonFile <- '/home/rstudio/projects/pg_plsda_classifier_operator/test.json'
  write(jsonData, jsonFile)
  
  
  # NOTE
  # It is unlikely that the processing takes over 10 minutes to finish,
  # but if it does, this safeguard needs to be changed
  x<-system2(MATCALL,
          args=c(MCR_PATH, " \"--infile=", jsonFile[1], "\""), timeout=600,
          stderr = TRUE, stdout = TRUE)
  
  print(x)
  outDf <- as.data.frame( read.csv(outfileDat) )
  outDf <- outDf %>%
    rename(.ci = colSeq) %>%
    rename(.ri = rowSeq) 
  
  classifierModel <- readBin(outfileVis, "raw", 10e6)
  
  
  
  outDf2 <- data.frame(
    model = "plsda_classifier",
    .base64.serialized.r.model = c(tim::serialise_to_string(classifierModel))
  )
  
  
  # Cleanup
  unlink(outfileVis)
  unlink(outfileDat)
  unlink(jsonFile)
  
  if(props$DiagnosticPlot != 'None'){
    output_string <- base64enc::base64encode(
      readBin(outfileImg, "raw", file.info(outfileImg)[1, "size"]),
      "txt"
    )
    
  
    output_md <- base64enc::base64encode(charToRaw("# Diagnostic Plot."),"txt")
    
    outTf <- tibble::tibble(
      filename = c("DiagnosticPlot.svg", "svg"),
      mimetype = c("text/markdown", 'image/svg+xml'),
      .content = c(output_md,output_string)
    )
    unlink(outfileImg)
    return( list(outDf, outDf2, outTf) )
  }else{
    return( list(outDf, outDf2) )
  }
}



# =====================
# MAIN OPERATOR CODE
# =====================
ctx = tercenCtx()


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


join1 = tbl1 %>% 
  ctx$addNamespace() %>%
  as_relation() %>%
  left_join_relation(ctx$crelation, ".ci", ctx$crelation$rids) %>%
  as_join_operator(ctx$cnames, ctx$cnames)

join2 = tbl2 %>% 
  ctx$addNamespace() %>%
  as_relation() %>%
  as_join_operator(list(), list())



if(props$DiagnosticPlot != 'None'){
  tbl3 <- tableList[[3]]  
  
  join3 = tbl3 %>% 
    ctx$addNamespace() %>%
    as_relation() %>%
    as_join_operator(list(), list())
  
  list(join1, join2, join3) %>%
    save_relation(ctx)
  
}else{
  list(join1, join2) %>%
    save_relation(ctx)
  
}


