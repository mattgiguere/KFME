function tsGenePool::createRandomChromosome
;function randomly sorts the gene and returns the new order
index = sort(randomu(seed,self.numGenes))
xPos = (*self.x)[index]
yPos = (*self.y)[index]
names = (*self.names)[index]
;create the new chromosome from the reorderd positions
randomChromosome = obj_new('tsChromosome',numGenes=self.numGenes, $
                            xPos=xPos, yPos=yPos, names=names)

return,randomChromosome
end

;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

function tsGenePool::init, numGenes
;create the gene pool with a set of random positions
;After they are created the positions never change, just the traversal order.
self.numGenes = numGenes
self.x = ptr_new(randomu(s,numGenes)*100.)
self.y = ptr_new(randomu(s,numGenes)*100.)
;when mating two chromosomes we need to be able to identify each gene.
;The next line creates random 10 character strings as the "city names"
self.names = ptr_new(string(byte(randomu(seed,10,numGenes)*26 + 97.0)))

return,1
end
;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

pro tsGenePool::cleanup
;free the pointers
if ptr_valid(self.x) then ptr_free,self.x
if ptr_valid(self.y) then ptr_free,self.y
if ptr_valid(self.names) then ptr_free,self.names

return
end

;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

pro tsGenePool__define

void = { tsGenePool, $
         numGenes : 0L, $ ;number of genes
         x : ptr_new(), $ ;x position
         y : ptr_new(), $ ;y position
         names : ptr_new() } ;unique name for identification
return
end