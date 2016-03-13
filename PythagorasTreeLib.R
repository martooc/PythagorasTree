#This program generates a Pythagoras tree with a randomly varying branching ratio at each bifurcation point

rotationMatrix2D = function(rad) matrix(c(cos(rad),-sin(rad),sin(rad),cos(rad)),2,2)

generatePythagorasTree <- function(maxLevel = 10, p = 0.5, random_p = FALSE, k=100){
  
  generateTreeRec <- function(level, anchor, angle, base, side, p){
    baseCube = matrix(c(0,0,1,1,0,1,1,0), 4, 2) * base
    R = rotationMatrix2D(angle)
    
    newCube = t((R %*% t(baseCube)) + anchor)
    
    if(level == maxLevel){
      return(c(as.vector(newCube), level))
    }else{
      #rbeta mean = shape1/(shape1+shape2)
      #if(random_p) p = rnorm(1,0.5,0.25) %% 1 #Little more peaked at 0.5. Truncated betwwen (0,1)
      if(random_p) p = rbeta(1, k * p, k * (1-p)) %% 1 #Little more peaked at 0.5. Truncated betwwen (0,1)
      angle_left = asin(sqrt(1-p))
      angle_right = asin(sqrt(p))
      base_left = base*sqrt(p)
      base_right = base*sqrt(1-p)
      
      return( rbind(c(as.vector(newCube), level), 
                    generateTreeRec(level + 1, newCube[2 + side,], 
                                    angle + ifelse(side==0, -angle_left, pi/2 - angle_left), 
                                    base_left, 0, p),
                    generateTreeRec(level + 1, newCube[3 + side,], 
                                    angle + ifelse(side==0, -(pi/2 - angle_right), angle_right), 
                                    base_right, 1, p)))
    } 
  }
  
  return(generateTreeRec(level=0, anchor=c(0,0), angle=0, base=1, side=0, p=p))
}

plotTree = function(tree, col1, col2, opacity){
  col2 = "#8B4513"
  col1 = "#00aa00"
  opacity = "ff"
  
  gr = paste(colorRampPalette(c(col2, col1))(nrow(tree)), opacity, sep = "")
  #PythagorasTree(n=9, f1=0.3, type=3, gradient= gr, border="#0000003f")
  
  plot(1, 1, type="n", xlim = range(tree[,1:4]), ylim = range(tree[,5:8]), asp=1)
  for (i in 1:nrow(tree)){
    lines(tree[i,c(1:4,1)], tree[i,c(5:8,5)],col=gr[i])
  }
}

tree = generatePythagorasTree(13, p=0.5, random_p=T, k = 100)
plotTree(tree)


(nrow(tree) - 1)
