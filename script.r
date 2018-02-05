options(warn=-1)
fileRda = "C:/Users/boefraty/projects/PBI/R/tempData.Rda"


saveResults2File = TRUE
saveResultsFile = "outData.csv"



if(file.exists(dirname(fileRda)))
{
  if(Sys.getenv("RSTUDIO")!="")
    load(file= fileRda)
  else
    save(list = ls(all.names = TRUE), file=fileRda)
}


set.seed(42)
source('./r_files/flatten_HTML.r')

############### Library Declarations ###############
libraryRequireInstall("ggplot2")
libraryRequireInstall("plotly")
libraryRequireInstall("distances")
libraryRequireInstall("DT")

####################################################

#PBI_PARAM 
w1 = 1
if(exists("mySettingsWeights_w1")){
  w1 = as.numeric(mySettingsWeights_w1)
}
w2 = 1
if(exists("mySettingsWeights_w2")){
  w2 = as.numeric(mySettingsWeights_w2)
}
w3 = 1
if(exists("mySettingsWeights_w3")){
  w3 = as.numeric(mySettingsWeights_w3)
}
w4 = 1
if(exists("mySettingsWeights_w4")){
  w4 = as.numeric(mySettingsWeights_w4)
}
w5 = 1
if(exists("mySettingsWeights_w5")){
  w5 = as.numeric(mySettingsWeights_w5)
}
w6 = 1
if(exists("mySettingsWeights_w6")){
  w6 = as.numeric(mySettingsWeights_w6)
}
w7 = 1
if(exists("mySettingsWeights_w7")){
  w7 = as.numeric(mySettingsWeights_w7)
}
w8 = 1
if(exists("mySettingsWeights_w8")){
  w8 = as.numeric(mySettingsWeights_w8)
}
distancesToLabeled = TRUE
if(exists("mySettingsViz_distancesToLabeled")){
  distancesToLabeled = mySettingsViz_distancesToLabeled
}

distancesToUnlabeled = TRUE;
if(exists("mySettingsViz_distancesToUnlabeled")){
  distancesToUnlabeled = mySettingsViz_distancesToUnlabeled
}
origFeatures = TRUE; 
if(exists("mySettingsViz_origFeatures")){
  origFeatures = mySettingsViz_origFeatures
}

clusteringResults = TRUE;
if(exists("mySettingsViz_clusteringResults")){
  clusteringResults = mySettingsViz_clusteringResults
}

compensate = TRUE;
if(exists("mySettingsKNN_compensate")){
  compensate = as.numeric(mySettingsKNN_compensate)
}

K = 3;
if(exists("mySettingsKNN_K")){
  K = as.numeric(mySettingsKNN_K)
}

Kavg = 5;
if(exists("mySettingsKNN_Kavg")){
  Kavg = as.numeric(mySettingsKNN_Kavg)
}

numClusters = 3; 
if(exists("mySettingsClust_numClusters")){
  numClusters = as.numeric(mySettingsClust_numClusters)
}


if (!exists("settings_rdatatable_params_method"))
{
  settings_rdatatable_params_method = "1000";
}

if (!exists("settings_rdatatable_params_showColumnFilters"))
{
  settings_rdatatable_params_showColumnFilters = "top";
}

#mySettingsFilter
numRows = 5
if (exists("mySettingsFilter_numRows"))
{
  numRows=  as.numeric(mySettingsFilter_numRows)
}

#mySettingsFilter
reportShow = FALSE
if (exists("mySettingsFilter_show"))
{
  reportShow =  as.logical(mySettingsFilter_show)
}

tableShow = TRUE
if (exists("mySettingsViz_show"))
{
  tableShow =  as.logical(mySettingsViz_show)
}

plotShow = FALSE
if(!tableShow && ! reportShow)
  plotShow = TRUE


keepOnly= "unlabeled"
if (exists("mySettingsFilter_keepOnly"))
{
  keepOnly=  mySettingsFilter_keepOnly
}
sortBy = "medianNN"
if (exists("mySettingsFilter_sortBy"))
{
  sortBy=  mySettingsFilter_sortBy
}



rtype = "engagement"
if (exists("mySettingsFilter_rtype"))
{
  rtype=  mySettingsFilter_rtype
}

plotDim = 2 #TODO
if (exists("mySettingsPlot_plotDim"))
{
  plotDim=  as.numeric(mySettingsPlot_plotDim)
}

# plotShow = TRUE #TODO
# if (exists("mySettingsPlot_plotShow"))
# {
#   plotShow =  mySettingsPlot_plotShow
# }

##############################
MakeSureNoDuplicates = function(Features, Weights)
{
  dd = duplicated(as.data.frame(Features))
  
  if(sum(dd) > 1)
  {
    Features = cbind(Features, rnorm(nrow(Features), mean = 0, sd = 0.1))
    Weights = c(Weights,0.001)
  }
  
  resList = list(Features = Features, Weights = Weights)
  return(resList)
}

MyStandardize = function(Fe)
{
  A = as.data.frame(scale(Fe))
  return(A)
  
}





MyCol2RGB = function(colName)
{ return(rgb(col2rgb(colName)[1]/255,col2rgb(colName)[2]/255,col2rgb(colName)[3]/255))}

#paste tooltips together separated by <br>
generateNiceTooltips = function(dataset)
{
  myNames = names(dataset)
  LMN = length(myNames)
  s = 1; if(LMN > 2)  s = 3
  
  nms = myNames[s:LMN]
  dta = dataset[,s:LMN]
  niceTooltips = NULL
  
  for (n in c(1:length(nms)))
  {
    if(length(nms) == 1)
      niceTooltips = paste(nms," = ", dta, sep = "") 
    else
    {
      niceTooltips = paste(niceTooltips,nms[n]," = ", dta[,n], sep = "")  
      if(n < length(nms))
        niceTooltips = paste(niceTooltips,"<br>", sep = "")
    }
  }
  return(niceTooltips)
} 




MySparsify = function(ID, Features,Labels,Tooltips, maxSize)
{
  nn = nrow(Features)
  ii = seq(1,length.out = nn)
  iiLab = ii[Labels]
  iiUnlab = ii[!Labels]
  
  
  
  
  
  
  if(nn > maxSize)
  {
    samplesLabeled = round(min(length(iiLab), maxSize*0.33))
    samplesUnlabeled = MAX_SIZE - samplesLabeled
    ii = c(sort(sample(iiLab,samplesLabeled)),sort(sample(iiUnlab,samplesUnlabeled)))
    
    
  }
  if(!is.null(ID))
    ID = ID[ii]
  if(!is.null(Features))
    Features = Features[ii,,drop = FALSE]
  
  if(!is.null(Labels))
    Labels = Labels[ii]
  if(!is.null(Tooltips))
    Tooltips = Tooltips[ii,,drop = FALSE]
  
  re = list(ID = ID, Features = Features,Labels = Labels, Tooltips = Tooltips)
  return(re) 
  
}

ComputedWeightedPC1PC2 = function(ID, Features, Labels, Weights)
{
  NFeatures = as.data.frame(scale(Features))
  NC = ncol(Features)
  for (c in seq(1,NC))
    NFeatures[,c] = NFeatures[,c]*Weights[c] 
  
  pca <- prcomp(x = NFeatures, scale.=rep(FALSE,NC))
  outDF = data.frame(PC1 = pca$x[,1],PC2 = pca$x[,2])
}





ComputeWeightedDistance <- function(ID, Features, Labels, Weights, params = NULL, toScale = TRUE, K = K, avgK = avgK)
{
  # Weights as number of Labels
  Weights= Weights/sum(Weights)
  
  #Normalize = "studentize"
  #Normalize = "mahalanobize"
  Normalize= NULL
  
 
  
  FeaturesTemp = MyStandardize(Features)
  fw = MakeSureNoDuplicates(FeaturesTemp, Weights)
  FeaturesTemp = fw$Features
  WeightsTemp = fw$Weights
   
  DD = distances(data = FeaturesTemp, id_variable = NULL, dist_variables = NULL, 
                 normalize = Normalize, weights = WeightsTemp)
  # matDD = round(as.matrix(DD),2)
  
  NN = nrow(Features)
  NC = ncol(Features)
  
  # For each Labeled find K closest unlabeled + distances
  # For each Unlabeled find K closest labeled + distances 
  
  # For each Labeled find NK closest unlabeled + avg distance
  # For each Unlabeled find NK closest labeled + avg distances 
  allInd = seq(1,NN)
  labInd = allInd[as.logical(Labels)]
  nonlabInd = allInd[!as.logical(Labels)]
  
  outDF =  data.frame(matrix(ncol = K*4 + 2, nrow = NN))
  
  # maxRadius = sqrt(NC)
  # 
  # Alab = nearest_neighbor_search(DD, K , query_indices = seq(1,NN), search_indices = labInd, radius = maxRadius)
  # outDF[,seq(1,length.out = K)] = t(Alab)
  # 
  
  
  
  # Anonlab = nearest_neighbor_search(DD, K , query_indices = seq(1,NN), search_indices = nonlabInd, radius = maxRadius)
  # outDF[,seq(1 + 2*K,length.out = K)] = t(Anonlab)
  # 
  
  #TODO: distances  
  
  
  
  if(1)
    for (r in seq(1,NN))
    {
      # start_time <- Sys.time()
      
      rowD = distance_columns(DD, column_indices = r, row_indices = NULL)
      
      LL = Labels[-r]
      rInd = allInd[-r]
      
      #sort row
      re = sort(rowD, decreasing =  FALSE, index.return = TRUE )
      
      sortedLabId = setdiff(intersect(re$ix,labInd),r)
      sortedNonLabId = setdiff(intersect(re$ix,nonlabInd),r)
      
      #k = th nearest 
      nearestLab = rowD[sortedLabId[seq(1,length.out = K)]]
      nearestLabIndexes = ID[sortedLabId[seq(1,length.out = K)]]
      
      nearestNonLab = rowD[sortedNonLabId[seq(1,length.out = K)]]
      nearestNonLabIndexes = ID[sortedNonLabId[seq(1,length.out = K)]]
      
      # k avg distance
      nearestLabAvg = round(mean(rowD[sortedLabId[seq(1,length.out = avgK)]], na.rm = TRUE),3)
      nearestNonlabAvg = round(mean(rowD[sortedNonLabId[seq(1,length.out = avgK)]], na.rm = TRUE),3)
      
      outDF[r,] = c(nearestLab,nearestLabIndexes,nearestNonLab,nearestNonLabIndexes,nearestLabAvg,nearestNonlabAvg)
      
      
      # print(Sys.time() - start_time)
      
    }
  
  
  nn1 = paste("dist2labeled ", seq(1,K), sep ="#")
  nn2 = paste("nearest2labeled ", seq(1,K), sep ="#")
  nn3 = paste("dist2unlabeled ", seq(1,K), sep ="#")
  nn4 = paste("nearest2unlabeled ", seq(1,K), sep ="#")
  nn5 = paste("K-NN dist2labeled ")
  nn6 = paste("K-NN dist2unlabeled ")
  colnames(outDF) =  c(nn1,nn2,nn3,nn4,nn5,nn6)
  outDF = cbind(Labels = Labels, outDF)
  
  outList = list(outDF = outDF, outDD = DD)
  return(outList)
  
}

SelectColumns = function(Values, Features, ID, clusters,vecClusterScores, distancesToLabeled, distancesToUnlabeled, 
                         origFeatures,clusteringResults, K)
{
  NC = ncol(Features)
  
  
  
  if(clusteringResults)
    Values = cbind(Values, cluster = clusters, clusterQuality = vecClusterScores)
  
  if(origFeatures)
    Values = cbind(Values,Features)
  
  excl = NULL
  if(!distancesToLabeled)
    excl = c(excl, seq(1,2*K)+1, 2*K + 2 + 2*K  )
  
  if(!distancesToUnlabeled)
    excl = c(excl, seq(1,2*K)+1 + 2*K,2*K + 3 + 2*K )
  
  Values[,excl] = NULL
  
  return(Values)
  
  
  
}

SelectAndSortRows = function(Tooltips, numRows,keepOnly,sortBy,Values, Features, ID, clusters, vecClusterScores,nnAvgDist,nnDist)
{
  NN = nrow(Values)
  ii = seq(1,NN)
  
  if(keepOnly %in% c("unlabeled"))
  {
    ii = ii[!Values$Labels]
  }
  if(keepOnly %in% c("labeled"))
  {
    ii = ii[Values$Labels]
  }
  
  
  if(sortBy == "NN")# sort by NN
  {
    iiOrder = order(nnDist)
    ii = intersect(iiOrder,ii)
  }
  
  if(sortBy %in% c("avgNN","medianNN"))
  {
    iiOrder = order(nnAvgDist)
    ii = intersect(iiOrder,ii)
  }
  
  if(sortBy %in% c("clusterScore"))
  {
    iiOrder = order(vecClusterScores, decreasing = TRUE)
    ii = intersect(iiOrder,ii)
  }
  
  Values = Values[ii,]
  if(!is.null(Tooltips))
    Tooltips = Tooltips[ii,,drop = FALSE]
  
  if(nrow(Values) > numRows)
  { 
    Values = Values[seq(1,numRows),,drop = FALSE]
    if(!is.null(Tooltips))
      Tooltips = Tooltips[seq(1,numRows),,drop = FALSE]
  }
  
  
  
  if(!is.null(Tooltips))
  {
    Values = data.frame(Rank = seq(1,numRows))
    Values = cbind(Values,Tooltips)
  }
  
  
  
  return(Values)
}


MAX_SIZE = 500 # TODO


if(!exists('Values'))
  Values = NULL

if(!exists('ID'))
  ID = NULL

if(!exists('Labels'))
  Labels = NULL

if(!exists('Tooltips'))
  Tooltips = NULL

Features = Values
NN = nrow(Features)
if(is.null(ID)){
  ID = seq(1,NN)
}else{
  ID = as.character(ID[,1])}

if(is.null(Labels)){
  Labels = as.logical(rep(1,NN))
}else{
  Labels = as.logical(Labels[,1])}

# SPARSIFY 
re = MySparsify(ID, Features,Labels,Tooltips, MAX_SIZE)
ID = re$ID
Features = re$Features
Labels = re$Labels
Tooltips = re$Tooltips 



NC = ncol(Features)
Weights = c(w1,w2,w3,w4,w5,w6,w7,w8)
Weights = Weights[seq(1,length.out = NC)] + 0.0000001


badCols = seq(1,NC)[(sapply(Features,var)<0.001)]
if(length(badCols))
{
  Features[,badCols] = NULL
  Weights = Weights[-badCols]
  NC = ncol(Features)
}


avgK = Kavg


res = ComputeWeightedDistance (ID, Features, Labels, Weights, K = K, avgK = avgK)
Values = res$outDF

clusters = NULL
if(clusteringResults)
{# clustering 
  hmethod = "ward.D2" #"complete"
  
  H.fit <- hclust(as.dist(res$outDD), method = hmethod)
  clusters <- cutree(H.fit, k = numClusters) # cut tree into  clusters
  uclusters= unique(clusters)
  numInClustersL = as.numeric(uclusters)
  numInClustersU = as.numeric(uclusters)
  for (cl in uclusters)
  {
    numInClustersL[cl] = sum((clusters == cl) & Labels)
    numInClustersU[cl] = sum((clusters == cl) & !Labels)
  }
  clusterScoreL = (numInClustersL/sum(Labels))/(numInClustersU/sum(!Labels) + numInClustersL/sum(Labels)) 
  vecClusterScores = clusterScoreL[clusters]
}

nnDist = Values[,2]
nnAvgDist = Values[,2 + 4*K]


if(saveResults2File && (Sys.getenv("RSTUDIO")!=""))
{
  clustersDF = data.frame(cluster = clusters, clusterQuality = vecClusterScores)
  df2save =  cbind(ID, Values, clustersDF, Features)
  write.csv(df2save, file = saveResultsFile, sep = ",")
  
}



# columns selection
Values = SelectColumns(Values, Features, ID, clusters, vecClusterScores, distancesToLabeled, distancesToUnlabeled, 
                       origFeatures,clusteringResults, K)

if(tableShow && !is.null(Tooltips))
  Values = cbind(Tooltips, Values)
  


if(reportShow)
  Values = SelectAndSortRows(Tooltips,numRows,keepOnly,sortBy,Values, Features, ID, clusters, vecClusterScores,nnAvgDist,nnDist)



if(rtype == "engagement")
{
  arrayLead = c(1,2,3,4)
  rgbLead = c(MyCol2RGB("green3"),MyCol2RGB("green2"),MyCol2RGB("green1"),MyCol2RGB("palegreen"))
}else
{
  arrayLead = c(1,2,3)
  rgbLead = c(MyCol2RGB("red2"),MyCol2RGB("red1"),MyCol2RGB("tomato"))
}





if(tableShow  || reportShow)
{
  p <- datatable(Values, 
                 class = 'cell-border compact stripe hover', 
                 rownames = FALSE, 
                 filter = settings_rdatatable_params_showColumnFilters, 
                 width = "100%", 
                 extensions = list('Scroller'), 
                 style = 'default',
                 options = list(
                   pageLength = settings_rdatatable_params_method,
                   lengthMenu = c(5, 10, 25, 50, 100, 1000),
                   autoWidth = FALSE,
                   scrollX = "100%",
                   scrollY = "50vh",
                   scroller = TRUE
                 )
  )  
  if('Rank' %in% colnames(Values))
    p <-p %>% formatStyle('Rank', target = 'row',
                          backgroundColor = styleEqual(arrayLead, rgbLead))
}

if(plotShow)
{
  library(MASS)
  matDD = as.matrix(res$outDD)
  dst = matDD 
  mds = isoMDS(dst, maxit = 5, tol = 0.5)
  cols = NULL
  mycolors = c("blue","green", "black", "blue","red", "purple", "orange", "cyan", "brown")
  cols = mycolors[clusters]
  pchs = NULL
  pchs[Labels] = 8
  pchs[!Labels] = 1
  transparency = 0.25
  pntSize = 1
  # plot as scatter 
  g <- ggplot(data=as.data.frame(mds$points), 
              aes( x = V1, y = V2)) +  
    geom_point(shape = pchs, colour = alpha(cols,transparency), size = pntSize * 2 ) + 
    xlab("X") + ylab("Y") +
    theme(legend.position = "none")
  
  g = g + labs (title = NULL, caption = NULL) + theme_bw() 
  
  
  #plot(mds$points, pch = pchs, cex = 1, col = adjustcolor(cols, alpha = 0.3), xlab = "X", ylab = "Y") 
  
  p=ggplotly(g)
  
 
    layerScatter = 1 # first layer is scatter 
    ntt = generateNiceTooltips(cbind(Features, Tooltips))
    #tooltips on scatter
    p$x$data[[layerScatter]]$text = ntt
  
  
  
}




############# Create and save widget ###############
internalSaveWidget(p, 'out.html');
#htmlwidgets::saveWidget(p, file = 'out.html', selfcontained = F)
####################################################

if(Sys.getenv("RSTUDIO")!="")
  print(p)