# This program generates a Pythagoras tree with a randomly varying branching ratio 
# at each bifurcation point


rotationMatrix2D <- function(rad){
  # Creates a 2D rotation matrix by rad radians
  #
  # Args:
  #   rad: rotation angle in radians
  #
  # Returns:
  #   2x2 rotation matrix
  matrix(c(cos(rad), -sin(rad),
           sin(rad),  cos(rad)),2,2)
} 


generatePythagorasTree <- function(maxLevel = 10, p = 0.5, random_p = FALSE, k=100){
  # Generates a Pythagoras tree
  #
  # Args:
  #   maxLevel: depth of the binary tree
  #   p: branching ratio from 0 to 1
  #   random_p: whether the p should be random at each split. Overrides parameter p
  #   k: Beta distribution parameter. Only relevant if random_p == TRUE
  #
  # Returns:
  #   Matrix representing the tree. Each row is one cube. Columns are the coordinates of
  #   the corners of the cube and the depth in the following order 
  #   (x1, x2, x3, x4, y1, y2, y3, y4, level)
  
  generateTreeRec <- function(level, anchor, angle, base, side, p){
    # Nested recursive function that computes the coordinates for each cube
    
    baseCube = matrix(c(0,0,1,1,0,1,1,0), 4, 2) * base
    R = rotationMatrix2D(angle)
    
    newCube = t((R %*% t(baseCube)) + anchor)
    
    if(level == maxLevel){
      return(c(as.vector(newCube), level))
      
    }else{
      # Generate random branching ratio. Biased towards the value that was used in last iteration
      if(random_p) p = rbeta(1, k * p, k * (1-p)) %% 1
      
      #Parameters For the next branch
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


plotTree = function(tree, col1, col2, alpha){
  # Plots the tree returned from generatePytahgorasTree
  #
  # Args:
  #   tree: tree that is returned from generatePythagorasTree
  #   col1: color of the trunk
  #   col2: color of a leaf
  #   alpha: alpha value of col1 and col2
  
  # Set the margins to 0
  p = par(mar = c(0,0,0,0))
  
  # Create a color palette
  gr = paste(colorRampPalette(c(col1, col2))(max(tree[,9] + 1)), alpha, sep = "")
  
  # Set up an empty plot
  plot(1, 1, type="n", xlim = range(tree[,1:4]), ylim = range(tree[,5:8]), asp=1)
  
  # One by one draw in the cubes
  for (i in 1:nrow(tree)){
    polygon(tree[i,c(1:4,1)], tree[i,c(5:8,5)], col=gr[tree[i,9] + 1], border="#0000003f")
  }
  
  # Restore the borders
  par(p)
}

# Generate a random tree with depth 10 and not too much variability (k=100)
tree = generatePythagorasTree(10, random_p=T, k = 100)

col1 = "#8B4513"
col2 = "#00aa00"
alpha = "bb"
plotTree(tree, col1, col2, alpha)


