library(tercen)
library(tercenApi)
library(dplyr, warn.conflicts = FALSE)
library(jsonlite)

library(tim)

MCR_PATH <- "/opt/mcr/v99"
MATCALL  <- "/mcr/exe/run_plsda.sh"
# =============================================



get_operator_props <- function(ctx, imagesFolder){
  MaxComponents <- -1
  Permutations   <- -1
  
  AutoScale <- "yes"
  Bagging <- "Bootstrap"
  NumberOfBags <- -1
  CrossValidation <- "LOOCV"
  Optimization<- "auto"
  QuantitationType <- "median"
  
  
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
  
  jsonFile <- tempfile(fileext = ".json")
  write(jsonData, jsonFile)
  
  system2(MATCALL,
          args=c(MCR_PATH, " \"--infile=", jsonFile[1], "\""))
  # 
  outDf <- as.data.frame( read.csv(outfileDat) )
  outDf <- outDf %>%
    rename(.ci = colSeq) %>%
    rename(.ri = rowSeq) 
  
  classifierModel <- readBin(outfileVis, "raw", 10e6)
  
  
  
  outDf2 <- data.frame(
    model = "model1",
    .base64.serialized.r.model = c(tim::serialise_to_string(classifierModel))
  )
  
  
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
  as_relation() %>%
  left_join_relation(ctx$crelation, ".ci", ctx$crelation$rids) %>%
  as_join_operator(ctx$cnames, ctx$cnames)

join2 = tbl2 %>% 
  ctx$addNamespace() %>%
  as_relation() %>%
  as_join_operator(list(), list())

join2 %>%  
  save_relation(ctx)

