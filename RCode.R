#setwd("C:\\Users\\Abhi\\Box Sync\\Academics\\Data Mining\\Homework1\\fifth\\hulk")
setwd("I:/Academics/Data Mining/Homework1/fifth/hulk")
col_names<-c('user','movie','rating','timestamp')
u1train<-read.table("u1.base",col.names=col_names)
u1test<-read.table('u1.test',col.names=col_names)

#trivial classifier - Mean Absolute Difference (MAD)
tri_mean_rating<-data.frame(tapply(u1train$rating,u1train$movie,mean,simplify=T))
tri_mean_rating$movie<-row.names(tri_mean_rating)
colnames(tri_mean_rating)<-c('mean_rating','movie')
u1test_final<-merge(u1test,tri_mean_rating,by.x="movie",by.y="movie")
b<-apply(u1test_final,1,function(x) abs(x[[3]]-x[[5]]))
MAD_trivial_classifier<-sum(b)/length(b)

#Generating ratings for test dataset using Euclidean distance
closeness_list<-list()
length(closeness_list)<-1
u1test_predicted<-data.frame(matrix(0,nrow(u1test),3))
for (i in 1:nrow(u1test)){
        user_selected<-u1test[[1]][i]
        movie_selected<-u1test[[2]][i]
        u1test_predicted[[1]][i]=u1test[[1]][i]
        u1test_predicted[[2]][i]=u1test[[2]][i]
        movie_selected_watched_users<-u1train[u1train[[2]]==movie_selected,1]
        user_movies_list<-u1train[u1train$user==user_selected,2]
        closeness_list[[1]]<-as.data.frame(matrix(NA,nrow=length(movie_selected_watched_users),ncol=2))
        if (length(movie_selected_watched_users) > 3){
                l=1
                for (j in movie_selected_watched_users){
                        similar_movies_list<-u1train[u1train$user==j,2]
                        common_movies_list<-intersect(user_movies_list,similar_movies_list)
                        sum=0
                        count=0
                        for (k in common_movies_list){
                                a<-u1train[u1train$user==user_selected & u1train$movie==k,3]
                                b<-u1train[u1train$user==j & u1train$movie==k,3]
                                sum=sum+(a-b)^2
                                count=count+1
                        }
                        
                        closeness_list[[1]][[1]][l]=j
                        closeness_list[[1]][[2]][l]=sqrt(sum)/count
                        l=l+1
                }
                closeness_list[[1]]<-closeness_list[[1]][order(-closeness_list[[1]][,2],na.last=TRUE),]
                Average_rating=mean(c(u1train[u1train[[1]]==closeness_list[[1]][[1]][1] & u1train[[2]]==movie_selected,3],u1train[u1train[[1]]==closeness_list[[1]][[1]][2] & u1train[[2]]==movie_selected,3],u1train[u1train[[1]]==closeness_list[[1]][[1]][3] & u1train[[2]]==movie_selected,3],u1train[u1train[[1]]==closeness_list[[1]][[1]][4] & u1train[[2]]==movie_selected,3]))
                u1test_predicted[[3]][i]=Average_rating
        }
        else{
                sum=0
                count=0
                for (j in movie_selected_watched_users){
                        sum=sum+u1train[u1train[[1]]==j & u1train[[2]]==movie_selected,3]
                        count=count+1      
              }
             Average_rating=sum/count
             u1test_predicted[[3]][i]=Average_rating
        }
        print(i)
}

#Using Euclidean distance - MAD
final_result<-abs(u1test_predicted[c(1:5),][[3]]-u1test[c(1:5),][[3]])
MAD_Euclidean_distance<-sum(final_result)/20000
        