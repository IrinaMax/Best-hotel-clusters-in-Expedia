# Best-hotel-clusters-in-Expedia
Best hotel clusters in Expedia

# R version of most popular local hotels
     library(data.table)
     exp_train_sorted <- fread("../input/train.csv", header=TRUE, select= c("is_booking","orig_destination_distance","hotel_cluster","srch_destination_id"))
     expedia_test <- fread("../input/test.csv", header=T,sep=",", stringsAsFactors = F)

     sum_and_count <- function(x){
       sum(x)*0.85 + length(x) *(1-0.85)
     }

     dest_id_hotel_cluster_count <- 
       exp_train_sorted[,sum_and_count(is_booking),by=list(orig_destination_distance, hotel_cluster)]
     dest_id_hotel_cluster_count1 <- 
        expedia_train[,sum_and_count(is_booking),by=list(srch_destination_id, hotel_cluster)]


     top_five <- function(hc,v1){
     hc_sorted <- hc[order(v1,decreasing=TRUE)]
     n <- min(5,length(hc_sorted))
     paste(hc_sorted[1:n],collapse=" ")
     }

     five_orig <- dest_id_hotel_cluster_count[,top_five(hotel_cluster,V1),by=orig_destination_distance]
     five_srch <- dest_id_hotel_cluster_count1[,top_five(hotel_cluster,V1),by=srch_destination_id]

     result_orig <- merge(expedia_test,five_orig, by="orig_destination_distance",all.x=TRUE)[order(id),list(id,V1)]

     result_srch <- merge(expedia_test,five_srch, by="srch_destination_id",all.x=TRUE)[order(id),list(id,V1)]

     result_orig$V1[is.na(result_orig$V1)] <- result_srch$V1[is.na(result_orig$V1)] 

     setnames(result_orig,c("id","hotel_cluster"))

     write.csv(result_orig, file='submission_combo_expedia.csv', row.names=FALSE)

