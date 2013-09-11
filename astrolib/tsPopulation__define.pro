;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

pro tsPopulation::TSPopSort, population
;sorts the population from the smallest distance to the greatest
numInPop = n_elements(population)
fitnessArray = fltarr(numInPop)
;get the fitness of each individual chromosome.
for i=0, numInPop-1 do $
  fitnessArray[i] = population[i]->getFitness()
indices = sort(fitnessArray)
population = population[indices]

return
end

;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

function tsPopulation::crossover, parents
;crossover breeding method

children = objarr(2)
;start off with the kids being the same as the parents
children[0] = parents[0]->copy()
children[1] = parents[1]->copy()

;only crossover if the randomness says so
if (randomu(seed,1))[0] lt self.crossOverRate then begin
  ;find out how long they are
  children[0] ->getProperty, numGenes=numGenes,xPos=momsX, yPos=momsY, $
                             names=momsNames
  children[1] ->getProperty, xPos=dadsX, yPos=dadsY, names=dadsNames
  ;mix the kids genes instead of the parents (identical at this point)
  crossOverPoint =  fix((randomu(seed,1) * numGenes)[0]) > 1
  ;get the initial points
  child1X = momsX[0:crossOverPoint-1]
  child1Y = momsY[0:crossOverPoint-1]
  child1names = momsNames[0:crossOverPoint-1]
  child2X = dadsX[0:crossOverPoint-1]
  child2Y = dadsY[0:crossOverPoint-1]
  child2names = dadsNames[0:crossOverPoint-1]
  ;now have to find the points that are missing from the other parent
  ;and in the order that they are present in the other parent
  missingNamesFromMom = momsNames[crossOverPoint:*]
  missingNamesFromDad = dadsNames[crossOverPoint:*]
  ;child 1 first
  numMissing = n_elements(missingNamesFromMom)
  indices = intarr(numMissing)
  for i=0,n_elements(missingNamesFromMom)-1 do begin
    indices[i] = where(missingNamesFromMom[i] eq dadsNames,count)
    if count ne 1 then stop ;should never happen
  endfor
  ;sort the indices in the order they came from dad
  indices = indices[sort(indices)]
  tempX = dadsX[indices]
  tempY = dadsY[indices]
  tempNames = dadsNames[indices]
  ;put the two arrays together to form the new sequence
  newXpos0 = [child1X,tempX] & newYpos0 = [child1Y,tempY]
  newNames0 = [child1Names,tempNames]
  ;now child 2
  numMissing = n_elements(missingNamesFromDad)
  indices = intarr(numMissing)
  for i=0,n_elements(missingNamesFromDad)-1 do begin
    indices[i] = where(missingNamesFromDad[i] eq momsNames,count)
    if count ne 1 then stop ;should never happen
  endfor
  ;sort the indices in the order they came from mom
  indices = indices[sort(indices)]
  tempX = momsX[indices]
  tempY = momsY[indices]
  tempNames = momsNames[indices]

  newXpos1 = [child2X,tempX] & newYpos1 = [child2Y,tempY]
  newNames1 = [child2Names,tempNames]
  ;give the new children their traversal order
  children[0]->setProperty,xPos=newXpos0, yPos=newYpos0, names=newNames0
  children[1]->setProperty,xPos=newXpos1, yPos=newYpos1, names=newNames1
endif

return, children
end
;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

function tsPopulation::mutate, children
;mutation method swaps two positions in each child

;only mutate if the randomness says so
if (randomu(seed,1))[0] lt self.mutationRate then begin
  ;find out how long they are
  children[0] ->getProperty, numGenes=numGenes,xPos=xPos0, yPos=yPos0, $
                             names=names0
  ;mix the kids genes instead of the parents
  randomPoint1 =  long((randomu(seed,1) * numGenes)[0])
  randomPoint2 =  long((randomu(seed,1) * numGenes)[0])
  children[1] ->getProperty, xPos=xPos1, yPos=yPos1, names=names1
  ;save the first point
  xRan0a = xPos0[randomPoint1]
  yRan0a = yPos0[randomPoint1]
  nameRan0a = names0[randomPoint1]
  ;replace it with the second
  xPos0[randomPoint1] = xPos0[randomPoint2]
  yPos0[randomPoint1] = yPos0[randomPoint2]
  names0[randomPoint1] = names0[randomPoint2]
  ;replace the second with the first
  xPos0[randomPoint2] = xRan0a
  yPos0[randomPoint2] = yRan0a
  names0[randomPoint2] = nameRan0a
  ;same sequence for the second child
  xRan1a = xPos1[randomPoint1]
  yRan1a = yPos1[randomPoint1]
  nameRan1a = names1[randomPoint1]
  xPos1[randomPoint1] = xPos1[randomPoint2]
  yPos1[randomPoint1] = yPos1[randomPoint2]
  names1[randomPoint1] = names1[randomPoint2]
  xPos1[randomPoint2] = xRan1a
  yPos1[randomPoint2] = yRan1a
  names1[randomPoint2] = nameRan1a
  ;update the chromosomes
  children[0] ->setProperty, xPos=xPos0, yPos=yPos0, names=names0
  children[1] ->setProperty, xPos=xPos1, yPos=yPos1, names=names1
endif

return, children
end
;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

function tsPopulation::selectParents
;uses tournament selection to pick the parents

;random spot to start looking
startIndex = (fix(randomu(seed,1) * self.populationSize))[0]
;handle the case where we are too close to the end
stopIndex = startIndex + self.tournamentSize-1
;check to make sure that we don't try running off the end of the array
if stopIndex gt (self.populationSize-1) then begin
   stopIndex = self.populationSize-1
   startIndex = stopIndex - self.tournamentSize
endif
;pull out the contestants
gladiators = (*self.population)[startIndex:stopIndex]
;find out who is best
self->TSPopSort,gladiators
;pick the best two
bestParents = gladiators[0:1]
return,bestParents
end

;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

pro tsPopulation::shuffle
;randomizes the order

;get the indices in random order
indices = sort(randomu(seed,self.populationSize))
;resort the population
*self.population = (*self.population)[indices]
return
end

;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

pro tsPopulation::reproduce
;method to create a new generation

;randomize the order
self->shuffle
;blank object array for kids
children = objarr(self.offspringPerGen)
;use crossover to beget children
for i=0,self.offspringPerGen-1,2 do begin ;2 kids for each crossover so skip by 2
  ;have a tournament to get the best parents
  parents = self->selectParents()
  ;breed the parents and get twins each time
  children[i:i+1] = self->crossover(parents)
  ;give an opportunity for mutations
  children[i:i+1] = self->mutate(children[i:i+1])
end
;find out who is best
population = *self.population
self->TSPopSort,population
;only keep the elitism best from the old population.
for i=self.elitism,n_elements(population)-1 do obj_destroy,population[i]
;replace the old population with the children. Note that if we have
;more children than populationSize - elitism they will be destroyed
count = 0L
for i = self.elitism,n_elements(population)-1 do begin
  population[i] = children[count]
  count = count + 1L
endfor
;destroy any remaining children
for i=count,self.offspringPerGen-1 do obj_destroy,children[i]
;update the population
*self.population = population

return
end

;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

pro tsPopulation::getBestChromosome,xPos=xPos, yPos=yPos, fitness=fitness
;gets the best current chromosome

;find out who is best
population = *self.population
self->TSPopSort,population
;the first is always the best
population[0]->getProperty,xPos=xPos, yPos=yPos, fitness=fitness

return
end

;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

function tsPopulation::init, initPopulationSize=initPopulationSize, $
                       populationSize=populationSize, oGenePool=oGenePool, $
                       offspringPerGen=offspringPerGen, elitism=elitism, $
                       tournamentSize=tournamentSize, crossOverRate=crossOverRate, $
                       mutationRate=mutationRate


;make an array for all the initial population chromosomes.
;By having a large initial array we cover more search space. Culling
;it down later speeds up the solution finding
oInitialPopulation = objarr(initPopulationSize)
for i=0,initPopulationSize-1 do $
  oInitialPopulation[i] = oGenePool->createRandomChromosome()

;find out how fit the initial population is. oInitialPopulation is scored
;and sorted in place.
self->TSPopSort,oInitialPopulation
newPopulation = oInitialPopulation[0:populationSize-1]
;release the other objects
for i=populationSize,initPopulationSize-1 do obj_destroy,oInitialPopulation[i]
self.populationSize = populationSize
self.population = ptr_new(newPopulation,/no_copy)
self.offspringPerGen = offspringPerGen
self.tournamentSize = tournamentSize
self.crossOverRate = crossOverRate
self.mutationRate = mutationRate
self.elitism = elitism

return,1
end

;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

pro tsPopulation::cleanup

if ptr_valid(self.population) then begin
  population = *self.population
  for i=0,n_elements(population)-1 do begin
    if obj_valid(population[i]) then obj_destroy,population[i]
  endfor
  ptr_free,self.population
endif

return
end

;{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|

pro tsPopulation__define
    void = {tsPopulation, $
            populationSize : 0L, $ ;size for each generation
            population : ptr_new(), $ ;pointer that holds the chromosomes
            offspringPerGen : 0L, $ ;how many kids each generation
            tournamentSize : 0L, $ ;how big each tournament round is
            crossOverRate : 0.0, $ ;how likely it is that two parents will mate
            mutationRate : 0.0, $ ;how often a mutation occurs
            elitism : 0L } ;how many individuals survive each generation

return
end
