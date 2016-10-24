# Pythagoras tree

Cool little script that generates Pythagoras trees with random branching

Usage:
* Generate a tree 
`tree <- generatePythagorasTree(10, random_p=T, k=100)`
* Define the end colors for a color gradient. `col1` for the trunk and `col2` for the leaves
`col1 <- "#8B4513"
 col2 <- "#00aa00"`
* Define the alpha level of the colors
`alpha <- "bb"`
* Plot the tree
`plotTree(tree, col1, col2, alpha)`

Sample image:
![A random Pythagoras tree](https://martooc.github.io/PythagorasTree/img/example-tree.png)