KFME
====

Keplerian Fitting Made Easy

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
https://github.com/mattgiguere/KFME

Getting Started
Additional documentation on how to setup and use KFME can be found at
https://sites.google.com/site/kfmeasy/
