
library(rmcfs)
library(devtools)
library(R.ROSETTA)
library(VisuNet)

data <- read.table("/media/akshai/New Volume/Uppsala_work_files/sem3/knowledge_based_systems/project/Project1.csv",header=TRUE)


mcfs_result <- mcfs(decision~., data, projections=auto,
               projectionSize=auto, splits=5, splitSetSize=0.66,
               cutoffPermutations = 20, threadsNumber = 8)


head(result$RI)
plot(result, type="distances")

dis_mcfs <- apply(mcfs_result$data[,1:length(mcfs_result$data[1,])-1], 2, function(x) cut(x, breaks=c(-Inf,0.2,0.8,Inf), labels = c(1,2,3))) 

transformed_data <-  as.data.frame(sapply(as.data.frame(dis_mcfs), as.numeric))

input_rosetta <- as.data.frame(cbind(dis_mcfs,mcfs_result$data[,dim(mcfs_result$data)[2]]))

rosetta_out <- rosetta(input_rosetta,discrete = TRUE)

saveRDS(output_rosetta, "/media/akshai/New Volume/Uppsala_work_files/sem3/knowledge_based_systems/project/output_rosetta.rds")

vis <- visunet(rosetta_out$main)

rosetta_out2 <- rosetta(input_rosetta,discrete = TRUE, roc = TRUE, clroc = "Age0To4")
rosetta_out3 <- rosetta(input_rosetta,discrete = TRUE, roc = TRUE, clroc = "Age28plus")
vis <- visunet(rosetta_out2$main)
plotMeanROC(rosetta_out2)
plotMeanROC(rosetta_out3)

# Genetic reducer
rosetta_genetic_out <- rosetta(input_rosetta,discrete = TRUE,reducer="Genetic")
saveRDS(output_rosetta_genetic, "/media/akshai/New Volume/Uppsala_work_files/sem3/knowledge_based_systems/project/output_rosetta_genetic.rds")
rosetta_fil <- rosetta_genetic_out$main[which(rosetta_genetic_out$main$pValue < 0.01),]
rosetta_fil2 <- rosetta_fil[with(rosetta_fil,order(accuracyRHS,supportRHS)),]
rosetta_fil3 <- rosetta_fil2[1:500,]

table(rosetta_fil3$decision)

vis <- visunet(rosetta_fil3)
