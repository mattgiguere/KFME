pro tsEvolve, numCities=numCities, initPopulationSize=initPopulationSize, $
              populationSize=populationSize, numGenerations=numGenerations, $
              offspringPerGen=offspringPerGen, tournamentSize=tournamentSize, $
              crossOverRate=crossOverRate, elitism=elitism,mutationRate=mutationRate
;Genetic evolution example.
;Loosely based upon "An introduction to genetic algorithms in Java" by
;Michael Lacy. In Java Developers Journal, volume 6,issue 3.
;
; keywords
; numCities = number of cities the salesman visits
; initPopulationSize = initial size of the population to cull from
; populationSize = size of each generation
; numGenerations = how long you want the process to evolve.
; offspringPerGen = how many kids are born each generation
; tournamentSize = how many parents compete for mating.
; crossOverRate = how likey it is that parents will mate (0 to 1)
; elitsm = how many parents survive to the next generation
; mutationRate = how likely it is that a child will have a mutation

if not keyword_set(numCities) then numCities = 25
if not keyword_set(initPopulationSize) then initPopulationSize = 500
if not keyword_set(populationSize) then populationSize = 100
if not keyword_set(numGenerations) then numGenerations = 5000
if not keyword_set(offspringPerGen) then offspringPerGen = 90
if not keyword_set(tournamentSize) then tournamentSize = 6
if not keyword_set(crossOverRate) then crossOverRate = 0.9
if not keyword_set(elitism) then elitism = 10
if not keyword_set(mutationRate) then mutationRate = 0.1

if (elitism + offspringPerGen) lt populationSize then begin
  void = dialog_message(['elitsm + offspringPerGen must be at least',$
                         'equal to or greater than the populationSize'])
  return
endif

;create the gene object. (Chromosomes are made up of genes)
oGenePool = obj_new('tsGenePool',numCities)
;create a population
oPopulation = obj_new('tsPopulation',initPopulationSize=initPopulationSize, $
                       populationSize=populationSize, oGenePool=oGenePool, $
                       offspringPerGen=offspringPerGen, elitism=elitism, $
                       tournamentSize=tournamentSize, crossOverRate=crossOverRate, $
                       mutationRate=mutationRate)

;evolve over the number of generations
window,0,xs=400,ys=400
window,1,xs=400,ys=400
window,xs=400,ys=400,/free,/pix
pix1 = !d.window
window,xs=400,ys=400,/free,/pix
pix2 = !d.window
for i=0,numGenerations-1 do begin
  oPopulation->reproduce
  oPopulation->getBestChromosome,xPos=xPos, yPos=yPos, fitness=fitness
  wset,pix1
  title = 'Generation = ' + string(i) + '  ' + string(fitness)
  plot,xPos,yPos,psym=-1,title=title,xrang=[0,100],yra=[0,100], $
       xstyle=1,ysty=1
  wset,0
  device,copy=[0,0,400,400,0,0,pix1]
  wset,pix2
  plot,findgen(10),xran=[0,numGenerations],yrange=[0,50000],xsty=1,$
                      ystyle=1,title='Fitness',/noerase
  plots,i,fitness,psym=1
  wset,1
  device,copy=[0,0,400,400,0,0,pix2]
endfor
;free the resources
obj_destroy, oGenePool
obj_destroy, oPopulation
wdelete,pix1, pix2
return
end