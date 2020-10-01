
library(rmcfs)

data <- read.table("/media/akshai/New Volume/Uppsala_work_files/sem3/knowledge_based_systems/project/Project1.csv",header=TRUE)


result <- mcfs(decision~., data, projections=auto,
               projectionSize=auto, splits=5, splitSetSize=0.66,
               cutoffPermutations = 20, threadsNumber = 8)


head(result$RI)
plot(result, type="distances")

dis_mcfs <- apply(mcfs_result$data[,1:length(mcfs_result$data[1,])-1], 2, function(x) cut(x, breaks=c(-Inf,0.2,0.8,Inf), labels = c(1,2,3))) 

transformed_data <-  as.data.frame(sapply(as.data.frame(dis_mcfs), as.numeric))

input_rosetta <- cbind(dis_mcfs,mcfs_result$data[,dim(mcfs_result$data)[2]])

output_rosetta <- rosetta(input_rosetta,discrete = TRUE)

output_rosetta_genetic <- rosetta(input_rosetta,discrete = TRUE,reducer="Genetic")

saveRDS(output_rosetta_genetic, "/media/akshai/New Volume/Uppsala_work_files/sem3/knowledge_based_systems/project/output_rosetta_genetic.rds")
saveRDS(output_rosetta, "/media/akshai/New Volume/Uppsala_work_files/sem3/knowledge_based_systems/project/output_rosetta.rds")
