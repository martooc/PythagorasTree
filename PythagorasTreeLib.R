#This program generates a Pythagoras tree with a randomly varying branching ratio at each bifurcation point

rotationMatrix2D = function(rad) matrix(c(cos(rad),-sin(rad),sin(rad),cos(rad)),2,2)

generatePythagorasTree <- function(maxLevel = 10, ratio = 0.5){
  
  generateTreeRec <- function(level, anchor, angle, baseLen, side){
    baseCube = matrix(c(0,0,1,1,0,1,1,0), 4, 2) * baseLen
    R = rotationMatrix2D(angle)

    newCube = t((R %*% t(baseCube)) + anchor)
    
    if(level == maxLevel){
      return(as.vector(newCube))
    }else{
      return( rbind(as.vector(newCube), 
                    generateTreeRec(level + 1, newCube[2 + side,], angle + (-1 + 2*side) * pi/4, baseLen/sqrt(2), 0),
                    generateTreeRec(level + 1, newCube[3 + side,], angle + (-1 + 2*side) * pi/4, baseLen/sqrt(2), 1)))
    } 
  }
  
  return(generateTreeRec(level=0, anchor=c(0,0), angle=0, baseLen=1, side=0))
}

plotTree = function(tree){
  plot(1, 1, type="n", xlim = range(tree[,1:4]), ylim = range(tree[,5:8]), asp=1)
  for (i in 1:nrow(tree)){
    lines(tree[i,c(1:4,1)], tree[i,c(5:8,5)])
  }
}




tree = generatePythagorasTree(3)
plotTree(tree)



