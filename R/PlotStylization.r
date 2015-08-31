##################################
# PlotStylization()              #
# ------------------------------ #
# Copyright (C) 2015 Aaron Albin #
# http://www.aaronalbin.com/     #
##################################

# This function takes a Stylization dataframe as input and draws it the information therein (e.g. the vertex locations and transition shapes) to draw the stylization in R's currently active window.
# It is assumed that this window contains an F0 track (x axis = time, y axis = F0) representing the same soundfile that the stylization is based on.
# This F0 track plot will generally be a rich visualization, created with the RichVisualization() function in this package, but PlotStylization() will work even if this is not the case.
# The stylization will be superimposed on top of this F0 track in the form of points and lines.

# This function is referenced on page 67 of:
# Albin, A. (2015). Typologizing native language influence on intonation in
#     a second language: Three transfer phenomena in Japanese EFL learners.
#     (Doctoral dissertation). Indiana University, Bloomington.
#     http://dx.doi.org/10.5967/K8JW8BSC

PlotStylization = function( # Begin argument list

Stylization # A stylization dataframe created with the Stylize() function

){ # End argument list; begin main function body

# Check to see whether a graphics device is open.
# If not, stop computation.
if( is.null( dev.list() ) ){ stop("No graphics devices are open. Create the F0 track plot first.") }

# Count how many transitions are represented in the Stylization
nTransitions = nrow(Stylization)-1
# Recall how any given vertex is included in the set of F0 points for both the movement to the left and the movement to the right.

# Thus paired into transitions, loop through the rows
for(EachTransition in 1:nTransitions){ # EachTransition=1

	#______#
	# TIME #

	# Extract the timestamps at the beginning and end of this transition
	CurrentStartingTime = Stylization[EachTransition,"Time_ms"]
	CurrentEndingTime = Stylization[EachTransition+1,"Time_ms"]

	# Use this information to make a series of 100 points across the transition
	# (The choice of the number 100 is of course arbitrary, but it suffices for making curves of sufficient resolution for plotting purposes.)
	TimeSeries = seq(from=CurrentStartingTime,to=CurrentEndingTime,length.out=100)

	# Also create a normalized version of this spanning from 0 to 1
	NormalizedTime = seq(from=0,to=1,length.out=100)

	#____#
	# F0 #

	# Determine the F0 values at the beginning and end of this transition
	CurrentStartingF0 = Stylization[EachTransition,"F0"]
	CurrentEndingF0 = Stylization[EachTransition+1,"F0"]

	# Calculate the magnitude of the resulting F0 movement
	RangeF0Movement = abs(CurrentStartingF0-CurrentEndingF0)

	# Set up a logical variable indicating whether this F0 movement is rising (TRUE) or falling (FALSE)
	RisingMovement = CurrentEndingF0 > CurrentStartingF0

	#_____________________#
	# THRESHOLD/GRADIENCE #

	# Extract out the threshold and gradience parameters associated with this transition
	CurrentThreshold = Stylization[EachTransition+1,"Threshold"]
	CurrentGradience = Stylization[EachTransition+1,"Gradience"]

	# Using the equations in (4) on p.79 of Albin (2015), convert these into the shape1 ('A') and shape2 ('B') parameters of the beta distribution
	shape1=(2*   CurrentThreshold )*CurrentGradience+1
	shape2=(2*(1-CurrentThreshold))*CurrentGradience+1
	# Solved in the opposite direction:
	# Gradience=(A/2)+(B/2)-1 # This can be simplified to Gradience=( (A+B)/2 ) - 1
	# Threshold=(A-1)/(A+B-2) # In more transparent form: Threshold=(A-1)/( (A-1) + (B-1) )

	#____________________#
	# ASSEMBLING CONTOUR #

	# Run the two shape parameters through pbeta() - the cumulative beta distribution function - to generate a curve shape
	BetaVector = pbeta(NormalizedTime, shape1=shape1, shape2=shape2)

	if(RisingMovement){ # If the F0 movement is a rise... 
		FlippedBeta = BetaVector # ...leave the curve shape as is
	}else{ # If, instead, it is a fall...
		FlippedBeta = ( -1 * BetaVector ) # ...flip it vertically (to make the curve shape a fall)
	}

	# Put the abstract curve shape into actual F0 coordinates (by adding the starting F0 value and stretching vertically by the size of the F0 movement)
	y = CurrentStartingF0 + (RangeF0Movement*FlippedBeta) 

	# Extract out from the Stylization dataframe what percent of the frames in this transition are voiced
	CurrentPercentVoiced = Stylization[EachTransition+1,"PercentVoiced"]

	# Bring everything up to this point together in a call to lines() to draw the transition lines
	lines( x=TimeSeries,
		   y=y,
		   col=grey(1-CurrentPercentVoiced), # In grey(), values closer to 0 are blacker, and values closer to 1 are whiter
		   lwd=CurrentPercentVoiced*2
	) # End call to lines()

} # End 'each transition'

# Finally, add the points for the vertices themselves
# This is performed at this point in the code (and not inside the for-loop above) so that the dots for the vertices appear *on top of* the transition lines.
points(x=Stylization$Time_ms, y=Stylization$F0, pch=21, bg="white",cex=1.5)

###############
#  ARGUMENTS  #
###############
# Stylization #
###############

} # End definition of function 'PlotStylization()'

########################################################################
# This program is free software: you can redistribute it and/or modify #
# it under the terms of the GNU General Public License as published by #
# the Free Software Foundation, either version 3 of the License, or    #
# (at your option) any later version.                                  #
#                                                                      #
# This program is distributed in the hope that it will be useful,      #
# but without any warranty; without even the implied warranty of       #
# merchantability or fitness for a particular purpose.  See the        #
# GNU General Public License for more details.                         #
#                                                                      #
# To receive a copy of the GNU General Public License, see:            #
# http://www.gnu.org/licenses/                                         #
########################################################################
