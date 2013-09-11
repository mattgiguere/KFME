;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

pro tsChromosome::setProperty, xPos=xPos, yPos=yPos, names=names
;replaces old traversal order with new one.

if n_elements(xPos) ne 0 then *self.xPos = xPos
if n_elements(yPos) ne 0 then *self.yPos = yPos
if n_elements(names) ne 0 then *self.names = names
return
end

;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

pro tsChromosome::getProperty, numGenes=numGenes, xPos=xPos, yPos=yPos, $
                               fitness=fitness, names=names
;gets these values.
numGenes = self.numGenes
if arg_present(xPos) then xPos = *self.xPos
if arg_present(yPos) then yPos = *self.yPos
if arg_present(fitness) then fitness = self->getFitness()
if arg_present(names) then names = *self.names

return
end

;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

function tsChromosome::copy
;returns and exact duplicate of the chromosome
chromosome = obj_new('tsChromosome',numGenes=self.numGenes, $
                            xPos=(*self.xPos), yPos=(*self.yPos), $
                            names=(*self.names))
return,chromosome
end

;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

function tsChromosome::getFitness
;calculates the total distance squared traveled

xPos = *self.xPos
yPos = *self.yPos
indices = lindgen(self.numGenes-1)
distance = total((xPos[indices+1]-xPos[indices])^2. + $
           (yPos[indices+1]-yPos[indices])^2.)

return, distance
end

;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

function tsChromosome::init,numGenes=numGenes, xPos=xPos, yPos=yPos, $
                            names=names
;initilizes a new chromosome based upon the input traversal order.
self.numGenes = numGenes
self.xPos = ptr_new(xPos)
self.yPos = ptr_new(yPos)
self.names = ptr_new(names)

return,1
end
;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

pro tsChromosome::cleanup
;frees pointers
if ptr_valid(self.xPos) then ptr_free,self.xPos
if ptr_valid(self.yPos) then ptr_free,self.yPos
if ptr_valid(self.names) then ptr_free,self.names

return
end

;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

pro tsChromosome__define

void = {tsChromosome, $
        numGenes : 0L, $ ;number of genes
        xPos : ptr_new(), $ ;x pos
        yPos : ptr_new(), $  ;y pos
        names : ptr_new() } ;names of each gene, used for identification

return
end