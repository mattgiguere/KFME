;+
;
;  NAME: 
;     kfme_init.pro
;
;  PURPOSE: 
;   Use this file for the initial setup of KFME on your machine. 
;	 All variables are defined below as comments on the line(s)
;	 preceding the variable that can be modified.
;
;  MODIFICATION HISTORY:
;        c. Matt Giguere 2011.11.15 05:51:13 PM
;
;-
pro kfme_init, $
datadir = datadir, $
kfmedir = kfmedir, $
outputdir = outputdir, $
starlist = starlist, $
winszx = winszx, $
winszy = winszy, $
winoffx = winoffx, $
winoffy = winoffy

;ROOTDIR: This will prefix the kfmedir, outputdir and starlist:
spawn, 'echo $home', mdir
rdir = mdir+'/'

;KFMEDIR: The directory that contains all the KFME IDL procedures
kfmedir = rdir+'projects/KFME/'

;OUTPUTDIR: The directory where all of the output should go (e.g.
;	plots, tables, IDL save structures)
outputdir = rdir+'kfme_output/'

;DATADIR: When clicking the "OPEN" button in KFME to open a new 
;IDL save structure
;datadir = '/Users/matt/data/CHIRPS/rvs'
datadir = '/Users/matt/projects/OTHER/N2K/PAPER2/data'

;WINSZ: Adjust the KFME window size. Use the fraction of your display:
winszx = 0.95
winszy = 0.95

;WINOFF: Adjust the X and Y offsets of the KFME window
winoffx = 0
winoffy = 0


;STARLIST: After the IDL save structure containing the velocities has been
;restored, STARLIST is a comma-delimited text file where each line represents
;a star that you would like the stellar parameters for. The parameters per line are
;1. The star name
;2. The Stellar Mass, in units of solar masses
;3. The uncertainty in the stellar masss, in units of solar masses
;4. The stellar radius, in solar radii
;5. The uncertainty in the stellar radius, in solar radii
starlist = rdir+'projects/KFME/starlist.txt'

end;kfme_init.pro
