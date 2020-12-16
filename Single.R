Single <- function(){

  SLM<-NULL
  
  tempMinDist = 0
  tempMinI = 0
  tempMinJ = 0
  
  inACluster = NULL
  
  while (length(inACluster)!=a[1]) {
    
    minDist = 100000000000
    
    varc = 1
    
    if(length(inACluster) != a[1]){
      while(is.na(match(varc,inACluster))==FALSE){
        varc = varc+1
      }
    }
    for(i in varc:a[1]){
      if(i == a[1]+1){
        break
      }
      
      for(j in 1:a[1]){
        
        if(j == a[1]+1){
          break
        }
        
        if(i == j){
          next
        }
        
        if((is.na(match(i,inACluster))==FALSE) && (is.na(match(j,inACluster))==FALSE)
           && (length(inACluster)!=a[1])){
          next
        }
        
        if(is.na(match(i,inACluster))==FALSE){
          
          clusterI = 10000000000
          trackerI = length(SLM)
          tpi = 0
          while(clusterI == 0){
            if(is.na(match(i,SLM[[trackerI]]))==FALSE){
              tempclusterI = 0
              
              for(o in 1:length(SLM[[trackerI]])){
                tempclusterI= 
                  (sqrt(sum(abs(my_data[j,] - my_data[SLM[[trackerI]][o],])^2)))
                if(tempclusterI<clusterI){
                  clusterI = tempclusterI
                  tpi = SLM[[trackerI]][o]
                }
              }
              trackerI=length(SLM[[trackerI]])
              
            }else{
              trackerI = trackerI-1
            }
          }
          tempMinDist = clusterI
          tpj = j
          
        }else if(is.na(match(j,inACluster))==FALSE){
          
          clusterJ = 10000000000
          trackerJ = length(SLM)
          tpj = 0
          while(clusterJ == 0){
            tempclusterJ = 0
            if(is.na(match(j,inACluster))==FALSE){
              for(o in 1:length(SLM[[trackerJ]])){
                
                tempclusterJ= 
                  (sqrt(sum(abs(my_data[i,] - my_data[SLM[[trackerJ]][o],])^2)))
                
                if(tempclusterJ<clusterJ){
                  clusterJ = tempclusterJ
                  tpj = SLM[[trackerJ]][o]
                }
              }  
              trackerJ=length(SLM[[trackerJ]])
            }else{
              trackerJ = trackerJ-1
            }
          }
          tempMinDist = clusterJ  
          tpi = i
        }else{
          
          tempMinDist = sqrt(sum(abs(my_data[i,] - my_data[j,])^2))
          tpi = i
          tpj = j
        }
        if(tempMinDist<minDist){
          minDist = tempMinDist
          tempMinI = tpi
          tempMinJ = tpj
        }
        
      }
      
    }
    
    if(is.na(match(tempMinI,inACluster)) && is.na(match(tempMinJ,inACluster))){
      inACluster = c(inACluster,tempMinI,tempMinJ)
      
      SLM[[length(SLM)+1]] = sort(c(tempMinI,tempMinJ),decreasing = FALSE) 
      
    }else if(is.na(match(tempMinI,inACluster))){
      
      jCluster = 0
      for(r in length(SLM):1){
        if(is.na(match(tempMinJ,SLM[[r]]))==FALSE){
          jCluster = r
          break
        }
      }
      SLM[[length(SLM)+1]] = sort(c(SLM[[jCluster]],tempMinI),decreasing = FALSE)
      inACluster = c(inACluster,tempMinI) 
      
      
    }else if(is.na(match(tempMinJ,inACluster))){
      
      iCluster = 0
      for(r in length(SLM):1){
        if(is.na(match(tempMinI,SLM[[r]]))==FALSE){
          iCluster = r
          break
        }
      }
      SLM[[length(SLM)+1]] = sort(c(SLM[[iCluster]],tempMinJ),decreasing = FALSE)
      inACluster = c(inACluster,tempMinJ)
    }
    
  }
  clusterLocation = NULL
  clusters = (1:a[1])
  counter = 1
  p = length(SLM)
  
  while(length(clusterLocation) != a[1]){
    if(is.na(match(clusters[counter],SLM[[p]]))==FALSE){
      clusterLocation = c(clusterLocation,p)
      p =length(SLM)
      counter = counter+1
    }else{
      p = p-1
    }
  }
  
  
  while(all(clusterLocation[1:a[1]] == clusterLocation[1])!= TRUE){
    
    tempMinDist = 0
    tempMinT = 0
    tempMinZ = 0
    tempclusterT = 0
    tempclusterZ = 0
    
    minDist = 10000000000
    
    for(t in 1:a[1]){
      
      if(t == a[1]+1){
        break
      }
      
      for(z in 1:a[1]){
        
        if(z == a[1]+1){
          break
        }
        
        if(t == z){
          next
        }
        
        clusterT = 0
        trackerT = length(SLM)
        
        while(clusterT == 0){
          if(is.na(match(clusters[t],SLM[[trackerT]]))==FALSE){
            clusterT=trackerT
          }else{
            trackerT = trackerT-1
          }
        }
        
        
        clusterZ = 0
        trackerZ = length(SLM)

        while(clusterZ == 0){
          if(is.na(match(clusters[z],SLM[[trackerZ]]))==FALSE){
            clusterZ=trackerZ
          }else{
            trackerZ = trackerZ-1
          }
        }
        
        if(clusterT == clusterZ){
          next
        }
        
        clusterT = 0
        tempclusterT = 0
        for(o in 1:length(SLM[[trackerT]])){
          tempclusterT= 
            (sqrt(sum(abs(my_data[z,] - my_data[SLM[[trackerT]][o],])^2)))
          
          if(tempclusterT<clusterT){
            clusterT = tempclusterT
          }
        }
        
        tempMinDist = clusterT
        if(tempMinDist<minDist){
          minDist = tempMinDist
          tempClusterT = trackerT
          tempClusterZ = clusterZ
        }
        
      }
    }
    
    SLM[[length(SLM)+1]] = sort(c(SLM[[tempClusterT]],SLM[[tempClusterZ]]),
                                decreasing = FALSE)
    
    clusterLocation = NULL
    clusters = (1:a[1])
    counter = 1
    p = length(SLM)
    
    while(length(clusterLocation) != a[1]){
      if(is.na(match(clusters[counter],SLM[[p]]))==FALSE){
        clusterLocation = c(clusterLocation,p)
        p =length(SLM)
        counter = counter+1
      }else{
        p = p-1
      }
    }
    
  }
  return(SLM)
}