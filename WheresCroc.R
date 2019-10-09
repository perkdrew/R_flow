library(WheresCroc)

myFunction = function(moveInfo, readings, positions, edges, matrices){
  
  #check whether tourist was devoured
  if(any(na.omit(positions[1:2]) < 0 )){
    cat("Tourist was devoured!")
    negNode = positions[which(na.omit(positions[1:2]) < 0) ]
    
    negNode = negNode * -1
    cat("Tourist was devoured at", negNode, "\n")
    priorState = matrix(0, ncol = 40)
    priorState[negNode] = 1
  }
  
  #check whether there's a movement vector 
  if(is.null(moveInfo$mem$tmatrix)){
    transitionM = transMatrix(1, edges)
    moveInfo$mem$tmatrix = transitionM
    
  }else{
    transitionM = moveInfo$mem$tmatrix
  }
  
  if(is.null(moveInfo$mem$priorState)){
    moveInfo$status = 0
    priorState = matrix(1/40,ncol = 40)
    
  }else{
    priorState = moveInfo$mem$priorState
  }
  
  #croc "sensor" readings
  #calculate the waterhole distributions of our emissions seperately
  salinity =  apply(matrices$salinity, MARGIN=1, function(row) dnorm(readings[1],row[1], row[2]) )
  phosphate = apply(matrices$phosphate, MARGIN=1, function(row) dnorm(readings[2],row[1], row[2]) )
  nitrogen =   apply(matrices$nitrogen, MARGIN=1, function(row) dnorm(readings[3],row[1], row[2]) )
  
  #calculate all of these distributions together as a probability
  probReadings = cbind(salinity,phosphate,nitrogen)
  probReadings = apply(probReadings, MARGIN=1, function(x) prod(x[1],x[2],x[3]))
  emissionM = diag(probReadings)
  
  
  ## CURRENT STATE ESTIMATION - FORWARD ALGORITHM w/ HMM 
  #define current states via matrix multiplication of prior states, transition matrix, and emission matrix
  currState = priorState %*% transitionM %*% emissionM
  moveInfo$mem$priorState =currState/sum(currState)
  cat("currState", currState, "\n")
  
  
  #implement shortest path heuristic to find croc
  probCrocPosition = which.max(currState)
  pathToCroc = shortestPath(positions[3], probCrocPosition, edges)
  
  
  #regulating movement
  nextMove1 = pathToCroc[1]
  nextMove2 = pathToCroc[2]
  
  
  #deciding which waterhole to search
  if(probCrocPosition == positions[3]){
    moveInfo$moves =c(0,0)
  }else if(nextMove2 == probCrocPosition){
    moveInfo$moves =c(nextMove1,0)
  }else{
    moveInfo$moves =c(nextMove1, nextMove2)
  }
  return(moveInfo)
}


#transition matrix of the croc grid
transMatrix = function(myPosition, edges){
  
  crocMatrix = matrix(nrow=40, ncol = 40)
  for(i in 1:40){
    neighbors = getNeighbors(i,edges)
    
    #0 out the probabilities of locations
    crocMatrix[i,] = 0
    
    #calculating the possibility of no movement
    crocMatrix[i,neighbors] = 1/(length(neighbors) +1)
  }
  return(crocMatrix)
}

#retrieve all the nodes and edges that leave from a given node
getNeighbors = function(node, edges) {
  c(edges[which(edges[,1] == node), 2], edges[which(edges[,2] == node), 1])
}

#place new node to the frontier sorted by the total cost of the path
sortByCost = function(newNode, frontier) {
  len = length(frontier);
  
  if(len == 0)
  {
    frontier[1] = list(newNode);
  }
  else{
    
    for(i in 1:len){
      if (frontier[[i]]$position == newNode$position){
        if (frontier[[i]]$cost >= newNode$cost){
          frontier = frontier[-i];
          len = len -1;
          break
          
        }else{
          return(frontier)
        }
      }
    }
    for (i in 1:len) {
      if(frontier[[i]]$cost  >= newNode$cost)
      {
        frontier = append(frontier, list(newNode), i-1);
        return(frontier)
      }
    }
    frontier[len +1] = list(newNode);
  }
  return(frontier)
}

# #shortest path heuristic 
shortestPath = function(trans,src,dest) {
  path=getPath(trans,src,dest,1,NULL,list())
  return(path[[2]])
}

getPath = function(trans,src,dest,dist,min,seq) {
  if(src==dest) {
    return(list(0,list(src)))
  }
  if(dist>10) {
    return(NULL)
  }
  
  l=trans[[src]]
  found=match(dest,l)
  if(!is.null(min)) {
    if(length(seq)>min) {
      return(NULL)
    } 
  }
  
  path=NULL
  if(!is.na(found)) {
    seq=append(seq,dest)  
    return(list(dist,seq))
  }
  else {
    numEdges=length(l)
    for(i in 1:numEdges) {
      if(l[[i]]==src) {}
      else if(is.na(match(l[[i]],seq))) {
        tseq=append(seq,l[[i]])
        tpath=getPath(trans,l[[i]],dest,dist+1,min,tseq)
        if(!is.null(tpath)) {
          if(is.null(min)) {
            path=tpath
            min=path[[1]]
          }
          else if(tpath[[1]]<min) {
            path=tpath
            min=path[[1]]
          }
        }
      }
    }
  }
  return(path)
}
