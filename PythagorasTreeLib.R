#This program generates a Pythagoras tree with a randomly varying branching ratio at each bifurcation point

rotationMatrix2D = function(rad) matrix(c(cos(rad),-sin(rad),sin(rad),cos(rad)),2,2)

generatePythagorasTree <- function(maxLevel = 10, p = 0.5, random_p = FALSE){
  generateTreeRec <- function(level, anchor, angle, base, side){
    baseCube = matrix(c(0,0,1,1,0,1,1,0), 4, 2) * base
    R = rotationMatrix2D(angle)
    
    newCube = t((R %*% t(baseCube)) + anchor)
    
    if(level == maxLevel){
      return(as.vector(newCube))
    }else{
      if(random_p) p = runif(1)
      angle_left = asin(sqrt(1-p))
      angle_right = asin(sqrt(p))
      base_left = base*sqrt(p)
      base_right = base*sqrt(1-p)
      
      return( rbind(as.vector(newCube), 
                    generateTreeRec(level + 1, newCube[2 + side,], 
                                    angle + ifelse(side==0, -angle_left, pi/2 - angle_left), 
                                    base_left, 0),
                    generateTreeRec(level + 1, newCube[3 + side,], 
                                    angle + ifelse(side==0, -pi/2 + angle_right, angle_right), 
                                    base_right, 1)))
    } 
  }
  
  return(generateTreeRec(level=0, anchor=c(0,0), angle=0, base=1, side=0))
}

plotTree = function(tree){
  plot(1, 1, type="n", xlim = range(tree[,1:4]), ylim = range(tree[,5:8]), asp=1)
  for (i in 1:nrow(tree)){
    lines(tree[i,c(1:4,1)], tree[i,c(5:8,5)])
  }
}

tree = generatePythagorasTree(13, p=0.5, random_p = T)
plotTree(tree)


