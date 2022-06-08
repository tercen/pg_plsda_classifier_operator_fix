library(tercen)
library(tercenApi)
library(dplyr, warn.conflicts = FALSE)
library(jsonlite)

library(tim)

MCR_PATH <- "/opt/mcr/v99"
MATCALL  <- "/mcr/exe/run_plsda.sh"
# =============================================

# http://127.0.0.1:5402/test-team/w/bddd3c84d46cb31435843a71c207aff5/ds/90a72c3d-c18c-4328-b1e2-20dce1052103
# options("tercen.workflowId" = "bddd3c84d46cb31435843a71c207aff5")
# options("tercen.stepId"     = "90a72c3d-c18c-4328-b1e2-20dce1052103")

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
    
    if (prop$name == "Diagnostic Plot"){
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
  outfileMat <- tempfile(fileext = ".mat")
  outfileTxt <- tempfile(fileext = ".txt")
  outfileImg <- tempfile(fileext = ".png")

  
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
    "OutputFileMat"=outfileMat, 
    "OutputFileTxt"=outfileTxt )
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
  
  write(jsonData, jsonFile)
  
  
  # NOTE
  # It is unlikely that the processing takes over 10 minutes to finish,
  # but if it does, this safeguard needs to be changed
  ec <- system2(MATCALL,
          args=c(MCR_PATH, " \"--infile=", jsonFile[1], "\""), timeout=600)

  # Error code 124 --> Timeout Happened
  if( ec == 124 ){
    stop(
      "Process Timed out\n
      \n
      HINT: Try increasing memory or CPU's for the operator"
    )
  }
  
  outDf <- as.data.frame( read.csv(outfileTxt) )
  outDf <- outDf %>%
    rename(.ci = colSeq) %>%
    rename(.ri = rowSeq) 
  
  classifierModel <- readBin(outfileMat, "raw", 10e6)
  
  
  
  outDf2 <- data.frame(
    model = "plsda_classifier",
    .base64.serialized.r.model = c(tim::serialise_to_string(classifierModel))
  )
  
  
  # Cleanup
  unlink(outfileMat)
  unlink(outfileTxt)
  unlink(jsonFile)
  
  if(props$DiagnosticPlot != 'None'){
    output_string <- base64enc::base64encode(
      readBin(outfileImg, "raw", file.info(outfileImg)[1, "size"]),
      "txt"
    )
    
  
    output_md <- base64enc::base64encode(charToRaw("# Diagnostic Plot."),"txt")
    
    outTf <- tibble::tibble(
      filename = c("DiagnosticPlot.png", "png"),
      mimetype = c("text/markdown", 'image/png'),
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

cTable <- ctx$cselect() %>%
  mutate_if(is.numeric, as.character)
rTable <- ctx$rselect() %>%
  mutate_if(is.numeric, as.character)


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
  
  list(join3, join1, join2) %>%
    save_relation(ctx)
  
}else{
  list(join1, join2) %>%
    save_relation(ctx)
  
}


