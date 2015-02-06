KFME
====

###Keplerian Fitting Made Easy

Keplerian Fitting Made Easy (KFME) is an IDL GUI for analyzing radial velocities in search of orbiting planets. First announced to public in Giguere et al. (2012), it was developed as part of a project for the Space Interferometry Mission (SIM) to simulate was SIM was capable of detecting. KFME uses a Levenber-Marquardt Least-Squares Minimization scheme to fit up to 7 planets simultaneously. It also includes many tools to visualize and perform statistical analysis on the data. Among the tools are
- Plotting of the radial velocity measurements as a function of time
- Plotting the orbit
- Plotting the residuals after subtracting the best-fit model
- Subtracting the theoretical curve of additional planets to see the affects of just one planet
- Phase-folding the data set to the orbital period of interest
- Periodogram analysis
- Residual periodogram analysis
- Periodogram False Alarm Probability (FAP) analysis
- Keplerian FAP analysis
- Bootstrap Monte Carlo analysis for uncertainty estimation
- LaTeX output of tables containing information on the star, predicted transit times and planetary orbital parameters
- Astrophysical jitter estimate input

KFME is open source, and the source code can be forked from github at
[https://github.com/mattgiguere/KFME](https://github.com/mattgiguere/KFME)

###Getting Started
Additional documentation on how to setup and use KFME can be found at
[https://sites.google.com/site/kfmeasy/](https://sites.google.com/site/kfmeasy/)

####Dependencies
- [IDLAstro](http://idlastro.gsfc.nasa.gov): GitHub repository [here](https://github.com/wlandsman/IDLAstro)
- [Coyote Library](http://www.idlcoyote.com/documents/programs.php#COYOTE_LIBRARY_DOWNLOAD): One of David Fanning's IDL Libraries.
- [idlutils](https://github.com/mattgiguere/idlutils): Some IDL utility files

####Setting Up an `idlkfme` Shell Startup Script
This party is optional, but can save a lot of time in the future. There is an executable shell script called `idlkfme` in the root KFME directory. Edit this to point to your versions of KFME, IDLAstro, coyote, and idlutils and it will take care of the pain of dealing with your IDL path in the future when using KFME. Here is what the `idlkfme` shell script looks like:

```sh
#!/bin/csh

#########################################################
#Keplerian Fitting Made Easy Configuration File
#########################################################

#Dependencies for KFME:
#1. KFME itself:
set KFME_DIR = ${HOME}/projects/KFME
#2. IDLAstro
set IDL_ASTRO = ${HOME}/projects/IDLAstro
#3. Coyote Library
set COYOTE = ${HOME}/projects/coyote
#4. idlutils
set IDL_UTILS = ${HOME}/projects/idlutils



if (-e /Applications/exelis) then
set IDL_DIR='/Applications/exelis/idl'
else
set IDL_DIR='/Applications/itt/idl/idl'
endif

setenv IDL_PATH +${KFME_DIR}:+${IDL_ASTRO}:+${COYOTE}:+${IDL_UTILS}:+${IDL_DIR}/lib
set PROPATH = ${KFME_DIR}

setenv IDL_STARTUP ${KFME_DIR}/.kfme_idl_startup.pro

echo "Now setting the path to "$IDL_PATH
echo "Now changing the directory to: "$PROPATH
cd $PROPATH
idl
````

The four lines you should edit are the four lines in the dependencies section (i.e. the lines starting with set KFME_DIR, set IDL_ASTRO, etc.)


I like to have all my shell script executables in one place (that is in my shell path), but I also want to make sure I have the latest version of the `idlkfme` shell script, so I created a symbolic link pointing from my ~/Scripts directory to my `idlkfme` script:

```sh
cd ~/Scripts
ln -s /Users/matt/projects/KFME/idlkfme idlkfme
```

Now that your `idlkfme` script is setup, simply type `idlkfme` at the command line and it should startup IDL in the KFME environment.

####Using KFME
Now that you are in IDL, simply type `kfme` and the IDL command line to start up KFME. If all goes well you should see the KFME GUI pop up  and it should look similar to the below image:

![KFME Startup GUI](https://github.com/mattgiguere/KFME/blob/master/images/KFME_Startup.png)

#####Importing Data

The default RV time series displayed is for Upsilon Andromeda. To open a new data set into KFME there are two options:

1. - Put the data into a CPS formatted CF3 IDL structure that is contained within an IDL save file
   - Click the "Open Data" button in the top right corner of the KFME GUI
   - Find the file in the dialog widget that pops up after clicking the "Open Data" button
   - Either double-click the filename, or highlight it and click the "OK" button in the bottom left

   ![KFME Import Data](https://github.com/mattgiguere/KFME/blob/master/images/KFME_Data_Import.png)


2. - Put the data into a delimited text file where the first three columns contain the observation times, the RV measurements, and the single measurement uncertainties
   - Specify the delimiter in the red circled field shown in the above figure. The default is a comma
   - Specify the number of rows to skip in the blue circled field shown in the above figure
   - Type in the offset from JD in the green circled field in the above figure. For example, if your time series is in JD - 2.44e6, enter 2.44d6 in this field).
   - The orange and purple fields show the units for the RV measurements and uncertainties, respectively.

#####Periodogram Analysis

Clicking on the "Periodogram" button will generate a Generalized Lomb-Scargle periodogram of the data.

#####False Alarm Probability Analysis

There are two different FAP methods built in to KFME: Periodogram FAP and Keplerian FAP analysis. To generate a periodogram FAP, enter the thresholds you would like to see plotted in the text field to the right of the "FAP Desired" text field.

To perform Keplerian FAP analysis, click the Kep FAP button.
