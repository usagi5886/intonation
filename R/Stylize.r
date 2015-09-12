##################################
# Stylize()                      #
# ------------------------------ #
# Copyright (C) 2015 Aaron Albin #
# http://www.aaronalbin.com/     #
##################################

# This function takes a set of vertices (F0 targets), uses this information to compute the shape of a schematic contour fit to the raw F0 data, and returns a dataframe describing the makeup of this contour.

# Input:

# This function takes as input information about the F0 turning points ('vertices') one hypothesizes to underlie the F0 track.
# The function allows for this information to be provided in two ways:
# (1) Interactively, by asking the user to select points in a currently open F0 track plot (x axis = time, y axis = F0)
# (2) Non-interactively, by providing (via the 'VertexIndices' argument) a vector indicating which frames in the Pitch object correspond to the vertices

# Output:

# As output, a specially structured dataframe is returned containing all the relevant information about the stylized F0 contour.
# Note that the row numbering begins at 0 since the first vertex in a contour is merely a 'starting value' and, as such, has no associated F0 transition.
# The stylization dataframe has the following columns:
# - Index: the index for this vertex (i.e. which frame in the Pitch object corresponds to this vertex)
# - F0: The F0 value (in Hertz) for this vertex
# - Time_ms: The timestamp (in millseconds) of this vertex
# - Threshold*: The threshold value of the F0 transition leading up to this vertex
# - Gradience*: The gradience value of the F0 transition leading up to this vertex
# - MAD: The intensity-weighted median absolute deviation, describing the goodness of fit of the stylization to the raw data (with smaller values indicating better fit)
# - PercentVoiced: A number between 0 (0%) and 1 (100%) indicating the proportion of the frames in the transition leading up to this vertex that are voiced

# * For further information on Threshold and Gradience, see section 3.4.3 on p.79ff of Albin (2015).

# This function is referenced on page 67 of:
# Albin, A. (2015). Typologizing native language influence on intonation in
#     a second language: Three transfer phenomena in Japanese EFL learners.
#     (Doctoral dissertation). Indiana University, Bloomington.
#     http://dx.doi.org/10.5967/K8JW8BSC

Stylize = function( # Begin argument list

# [1]
PitchPath, # Path to the Pitch object containing the F0 data, stored in Praat's (long) 'text' format.
# This is the only required argument to this function.
# Note that it is assumed that the F0 data in this Pitch object is the same data being displayed in the currently open plot.

# [2]
Message=TRUE, # If TRUE, the message 'Please select the contour vertices.' is displayed in the R console (to make it clear that user input is being requested).

# [3]
nVertices, # If left unspecified, when done selecting vertices (i.e. clicking F0 points), the user must right-click and select 'Stop'.
# Instead, 'nVertices' can be set to an integer specifying up front how many vertices will be selected (after which the interactive selection process is automatically terminated).
# The latter is useful in cases where, because of the structure of one's analysis, the same number of vertices should be selected for every soundfile.

# [4]
WavePath, # Path to a WAV file one one's computer containing the audio from which the Pitch object was derived.
# If specified, after each click during the stylization process, plays a short clip of that soundfile to help make it clear where the following vertex should be placed.

# [5]
PlayDuration_ms=1000, # The duration (in milliseconds) of the soundfile clip that should be played.
# Defaults to 1000 ms, i.e. 1 second.

# [6]
ZeroPad=TRUE, # Should a half-second of silence be appended to the end of the soundfile clip when played?
# Depending on one's computing environment, the play() function from the 'audio' package can occasionally have artifacts if not zero-padded at the end in this way.
# Switching 'ZeroPad' to FALSE makes it possible to create stylizations slightly faster (without a 500 ms delay after each click), but it might cause problems in playing the audio.

# [7]
ClickSound, # Whether a bell or other similar sound (depending on the operating system) should play after each click
# If WavePath is provided, defaults to FALSE (so as not to make it hard to hear the audio)
# If no WavePath is provided, however, defaults to TRUE (since the auditory feedback helps prevent cases where the user is unsure whether R successfully registered a click).

# [8]
VertexIndices, # A set of integers (in ascending order) indicating which frames in the Pitch object correspond to the vertices
# The F0 values in these frames must be non-NA.
# If 'VertexIndices' is provided, the interactive vertex-selection mode will be skipped, hence the arguments 'WavePath', 'PlayDuration_ms', 'ZeroPad', 'Message', and 'nVertices' are ignored (with a warning).

# [9]
nSeeds=10 # Number of seeds (initialization candidates) for exploring the search space during the non-linear optimization process.
# Any number between 1 and 100 is valid.
# Decreasing nSeeds makes the code run faster but makes it less guaranteed that an optimal fit will be obtained.
# Defaults to 10, i.e. the algorithm uses the top 10% of the 100 candidates in the 10x10 grid described on p.84-86 of Albin (2015).
# Note that, since 'nSeeds' is specified at the level of the Stylize() function as a whole, all F0 transitions in the contour will have the same nSeeds applied in the fitting process.

){ # End argument list; begin main function body

# Require audio package (for load.wave() and play() functions down below)
require("audio")

#_______________________#
# BRING IN PITCH OBJECT #

# Make sure 'PitchPath' points to an actual file
if(!file.exists(PitchPath)){
	stop("No file exists at the specified PitchPath.")
} # End 'if' statement
# Note that it is left un-verified whether the file at that path is actually a Pitch object.

Frames = ReadPitch( File=PitchPath, Return="Frames" ) # ReadPitch() is another function in this package.

# Extract out time, F0, and intensity information from the Pitch object
Time_s = Frames$Time # Units=seconds
Time_ms = Time_s*1000 # Change to milliseconds
F0 = Frames$Frequency
Intensity = Frames$Intensity

#__________________#
# INPUT VALIDATION #

# If 'nVertices' is provided, make sure it is numeric
if( !missing(nVertices) ){
	if( !is.numeric(nVertices)){
		stop("'nVertices' must be numeric (any positive integer).")
	} # End 'if nVertices is numeric'
} # End 'if nVertices is not missing'

# If 'WavePath' is provided, make sure it points to an actual file
if( !missing(WavePath) ){
	if(!file.exists(WavePath)){
		stop("No file exists at the specified WavePath.")
	} # End inner 'if' statement
} # End outer 'if' statement

# Checks on 'PlayDuration_ms' (but only if it is anything other than the default)
if( PlayDuration_ms!=1000 ){

	# If 'PlayDuration_ms' is less than 1, it is most likely an error (whereby the user has specified it in seconds, not milliseconds).
	# In such cases, issue an error.
	if( PlayDuration_ms<1 ){
		stop("'PlayDuration_ms' should be specified in milliseconds.")
	} # End 'if PlayDuration_ms' is less than 1

	# 'PlayDuration_ms' also shouldn't be longer than the entire soundfile's duration.
	# The code below clips the audio if it goes outside the time range of the soundfile, so this actually will not cause a problem, hence merely issue a warning.
	SoundfileDuration_ms = max(Time_ms)
	if(PlayDuration_ms > SoundfileDuration_ms){
		WarningMessage = paste("PlayDuration_ms is longer than the duration of the entire soundfile (",
								PlayDuration_ms,
								" vs. ",
								SoundfileDuration_ms,
								" ms).",
								sep="")
		warning(WarningMessage)
	} # End 'if play duration is longer than the soundfile duration'
} # End 'if PlayDuration_ms is something other than the default'

# Various checks on 'VertexIndices' (if manually specified)
if( !missing(VertexIndices) ){

	# Should be an integer, so check whether it is numeric
	if( !is.numeric(VertexIndices) ){stop("'VertexIndices' should be an integer vector.")}

	# Should be of length 2 or more (in order to create a the multiple points needed to form a contour in the first place)
	if( ! (length(VertexIndices) >= 2) ){stop("'VertexIndices' should be of length 2 or more")}

	# Should be in ascending order. This can easily be fixed, though, so if not, issue a warning and fix it.
	if( !identical(VertexIndices,sort(VertexIndices)) ){
		VertexIndices = sort(VertexIndices)
		warning("'VertexIndices' have been re-arranged in ascending order.")
	} # End 'if not in ascending order'

	# If 'Message', 'nVertices', 'WavePath', 'PlayDuration_ms', 'ZeroPad' and/or 'ClickSound' are also provided/non-default, ignore them (with a warning)
	if( Message==FALSE | !missing(nVertices) | !missing(WavePath) | PlayDuration_ms!=1000 | ZeroPad==FALSE | !missing(ClickSound)){
		MissingArguments_Vector = c("Message","nVertices","WavePath","PlayDuration_ms","ZeroPad","ClickSound")[c( Message==FALSE,!missing(nVertices),!missing(WavePath),PlayDuration_ms!=1000,ZeroPad==FALSE,!missing(ClickSound) )]
		MissingAruments_OneString = paste(MissingArguments_Vector,collapse=", ")
		warning(paste("Since 'VertexIndices' was provided, the following arguments have been ignored:\n       ",MissingAruments_OneString,sep=""))
	} # End 'if superfluous arguments are also provided'

	# All numbers in 'VertexIndices' must be less then or equal to the total number of frames in the pitch object.
	if( !( max(VertexIndices) <= length(F0) ) ){
		stop(paste("One or more of the VertexIndices extend outside the",length(F0),"frames in the Pitch object."))
	} # End 'if some VertexIndices are outside the range of frames in the Pitch object

	# The provided frame numbers must be all non-NA.
	if( sum( is.na( F0[VertexIndices] ) )>=1 ){
		stop("The VertexIndices must be all non-NA.")
	} # End 'if any VertexIndices are NA'

} # End 'if VertexIndices is not missing'

# nSeeds needs to be any integer from 1 to 100
if( !( nSeeds >= 1 & nSeeds <= 100 ) ){
		stop("nSeeds must be an integer between 1 and 100.")
} # End if nSeeds is outside [1,100]

#__________________#
# INTERACTIVE MODE #

if(missing(VertexIndices)){ # i.e. if the vertices need to be obtained from the user

	VertexIndices = vector(mode="integer") # Create empty vector

	# If Message is TRUE, show a message requesting the user to select vertices
	if(Message){
		cat("\n----------------------------------\nPlease select the contour vertices.\n----------------------------------\n\n")
		flush.console()
	} # End 'if Message' check

	# Determine whether to play the click sound
	if( missing(ClickSound) ){
		if( missing(WavePath) ){ # If no audio will be played...
			ClickSound=TRUE # ...it is safe to take advantage of the auditory feedback the bell provides
		}else{ # If audio *will* be played...
			ClickSound=FALSE # ...skip the click sound so as not to interfere with the actual soundfile audio itself
		} # End if/else WavePath is missing
	} # End 'if ClickSound is missing' (i.e. if the user didn't make a specific request about whether the click sound should be played)

	# Determine the original value of the 'locatorBell' option, before the Stylize() function was run
	OriginalBellSetting = options("locatorBell")

	# Set the option accordingly (depending on what the user requested via the 'ClickSound' argument)
	if(ClickSound){ # If playing the click sound
		options(locatorBell=TRUE) # Turn it off
	}else{ # If *not* playing the click sound
		options(locatorBell=FALSE) # Turn it off
	}# If turning

	repeat{ # Open a repeat-loop for the eliciting of points

		# Ask for a point from the user
		NewPoint = identify(x=Time_ms,y=F0,n=1,plot=FALSE)

		# When the user is done selecting points, they must right-click, and when they are prompted 'Stop' or 'Continue', they must hit 'Stop'.
		# If this happens, then break out of the repeat loop.
		if( identical( NewPoint, integer(0) ) ){break}

		# If a 'WavePath' was provided, play the soundfile to the user (to help them decide where to click)
		if( !missing(WavePath) ){

			# First load it in
			AudioObject = audio:::load.wave(WavePath)
			# Note: The load.wave() function comes from the 'audio' package (a dependency for this package)

			# Determine sample rate
			SampleRate = attributes(AudioObject)$rate

			# Start the playing from wherever the last click was
			StartPoint = round( Time_ms[NewPoint]/1000 * SampleRate )

			# Play either to:
			#   1) the last click plus the requested clip duration, or
			#   2) to the end of the file -
			# whichever comes first.
			EndPoint = min( c( StartPoint+ SampleRate * PlayDuration_ms/1000), # (1)
			                   length(AudioObject) # (2)
			              ) # End min()

			# Based on the start and end points thus determined, extract out the relevant subset of the audio
			SampleRange = StartPoint:EndPoint
			AudioClip = AudioObject[SampleRange]

			# To avoid a bug in playing audio on some systems, add a half-second of zero-padding to the end of the audio
			if(ZeroPad){
				ZeroPadding = rep(0,times=SampleRate/2)
				ZeroPadded = append(AudioClip,ZeroPadding)
				attributes(ZeroPadded) <- attributes(AudioObject)
			}else{ # i.e. if the audio should *not* be zero padded
				ZeroPadded=AudioClip # Trivially
			} # End if/else

			# Play the ultimate audio
			audio:::play(ZeroPadded)
			# Note: The play() function comes from the 'audio' package (a dependency for this package)

			# Tell R to pause ('sleep') while the audio is playing (to help prevent crashing).
			Sys.sleep(length(ZeroPadded)/SampleRate)

		} # End 'if WavePath is provided'

		# Now verify that it is of a higher index than the last one, and if not, enter the next loop and ask for another one.
		nThusFar = length(VertexIndices)
		if(nThusFar>=1){ # 
			if( NewPoint <= VertexIndices[nThusFar] ){ # The 'or equals to' catches times when the user accidentally clicks the same point twice
				next
			} # End 'if the new point's index is not after the last point's index'
		} # End 'if there are any previous points'

		# Append the new point
		VertexIndices = c(VertexIndices,NewPoint)

		# End the looping if the user specified a specific number of vertices to obtain
		if(!missing(nVertices)){ if( length(VertexIndices)==nVertices ){break} }

	} # End 'repeat' loop for the eliciting of points

	# Revert the 'locatorBell' option to its original setting (regardless of whether click sounds were played) 
	options(locatorBell=OriginalBellSetting)

} # End 'if VertexIndices is missing'

#____________________#
# INTERIM PROCESSING #

# Count how many vertices there are (either provided as an argument to Stylize() or via the interactive mode above)
nVertices = length(VertexIndices)

# Determine how many F0 transitions that number of vertices creates
nTransitions = nVertices-1

# Use the number of vertices to build an empty stylization dataframe
Stylization <- data.frame( Index = VertexIndices, # Indices
                           F0 = F0[VertexIndices], # F0 values
                           Time_ms = Time_ms[VertexIndices], # Time values
                           # The remaining four columns are filled with dummy NAs since they still need be filled in later down below
                           Threshold = rep(NA,times=nVertices),
                           Gradience = rep(NA,times=nVertices), 
                           MAD = rep(NA,times=nVertices), 
                           PercentVoiced = rep(NA,times=nVertices) )

#________________#
# CALCULATEMAD() #

# Define a function 'CalculateMAD()', which calculates the intensity-weighted median absolute deviation
# This function is used in two places in the code below (both the initialization and the subsequent optimization), hence it is specified here to avoid redundancy.
# (Accordingly, several of the variables inside this function are defined lower down below.)
CalculateMAD = function(GridRow){
	# The input 'GridRow' is a vector of length 2, representing a pairing of values for the threshold and gradience parameters
	Threshold = GridRow[1]
	Gradience = GridRow[2]

	# Using the equations in (4) on p.79 of Albin (2015), convert these into the shape1 ('A') and shape2 ('B') parameters of the beta distribution
	shape1=(2*   Threshold )*Gradience+1
	shape2=(2*(1-Threshold))*Gradience+1
	# Solved in the opposite direction:
	# Gradience=(A/2)+(B/2)-1 # This can be simplified to Gradience=( (A+B)/2 ) - 1
	# Threshold=(A-1)/(A+B-2) # In more transparent form: Threshold=(A-1)/( (A-1) + (B-1) )

	# Run these through the cumulative beta distribution (pbeta) function to obtain the raw contour
	RawContour = pbeta(q=NormalizedTime, shape1=shape1, shape2=shape2)
	# ('Normalized time' is a vector of values from 0 to 1.)
	
	if(RisingMovement){ 
	# 'RisingMovement' is a logical flag indicating whether the transition in question involves an F0 rise (TRUE) or an F0 fall (FALSE)
		ActualContour = FirstF0Point + RangeF0Movement * RawContour
		# This takes the raw contour and maps it onto the actual contour by adding the F0 value the contour started from ('FirstF0Point') and stretching it vertically according to how large the F0 movement was ('RangeF0Movement')
	}else{ # If a falling movement, do the same thing except flip it vertically (by subtracting rather than adding)
		ActualContour = FirstF0Point - RangeF0Movement * RawContour
	} # End 'if/else the F0 movement is rising'

	# Calculate the intensity-weighted median absolute deviation
	Output = median( IntensityVector * abs( F0Vector - ActualContour), na.rm=TRUE )

	# How the intensity weighting works:
	# For example, if the absolute deviation of a given frame is 5 Hz, then if that frame had 100% intensity it would be treated as a 5 for the purposes of calculating the median.
	# Alternatively, if that frame had 0% intensity, it would be treated as a 0.
	# Thus, deviations on loud frames make the resulting median skew toward larger values.
	# Since the optim() algorithm tries to minimize this number, this is exactly what we want.

	# Return the intensity-weighted MAD as output from the function
	return(Output)

} # End definition of function 'CalculateMAD()'

#_____________________#
# STYLIZE TRANSITIONS #

# Loop through transitions, one by one
for(EachTransition in 1:nTransitions){

	# Any given vertex is included in the set of F0 points for both the movement to the left and the movement to the right.
	# For example, if there are 13 points in the F0 contour (top row)...

	# .............
	# A-----B-----C

	# ...then the transition from A to B would include the first 7 points (including the vertices A and B).
	# Likewise, the transition from B to C would include the last 7 points (including the vertices B and C).

	# Extract out the index locations of the two vertices involved in this transition (i.e. the transition's beginning and ending point)
	LeftEdgeIndex  = VertexIndices[EachTransition  ]
	RightEdgeIndex = VertexIndices[EachTransition+1]

	# Make a vector of iteratively increasing integers extending from one edge index to the other
	IndexRange = LeftEdgeIndex:RightEdgeIndex

	# Use this to extract out the F0 values within the range of frames in question.
	# Note that this set of F0 values will often have NAs in it (but only in the middle, since, by design, the vertices themselves must be non-NA).
	F0Vector = F0[IndexRange]

	# Determine the number of frames that this transition spans (including frames whose F0 value is NA)
	nFrames = length(F0Vector)

	# Also extract out the intensity values for the same range of frames.
	# (The intensity information in a Praat Pitch object is always non-NA.)
	IntensityVector  = Intensity[IndexRange]
	# This variable is drawn upon in the CalculateMAD() function (defined up above)

	# Extract out the F0 values of the vertices at the beginning/end of this transition
	FirstF0Point = F0Vector[1]
	LastF0Point  = F0Vector[nFrames]

	# Create a logical variable for determining whether the transition is a rise or a fall
	RisingMovement = LastF0Point > FirstF0Point

	# Find out how many Hertz of an F0 movement is involved in this transition (in terms of absolute value, thus collapsing rises and falls)
	RangeF0Movement = abs(FirstF0Point-LastF0Point)

	# Make a vector of 'pseudo-time' from 0 to 1, with one point for each frame inside the transition in question
	NormalizedTime = seq(from=0,to=1,length.out=nFrames)

	# In the reparametrization in terms of threshold and gradience, Threshold ranges from 0 to 1, but use 0.01 to 0.99 instead to avoid edge artifacts.
	# Gradience, on the other hand, can vary from 0 to infinity, but use 0.01 rather than 0 itself because the latter leads to division by zero.

	# Make a coarse 10x10 grid of combinations of Threshold and Gradience parameters that spans a good portion of these theoretical limits
	Grid = expand.grid( Threshold = seq(from=0.01,to=0.99,length.out=10),
						Gradience = seq(from=0.01,to=5,   length.out=10))
	# Note that 0.01 and 0.99 go to two decimal places

	# Calculate the intensity-weighted median absolute deviation (MAD) for all 100 of these candidates
	MADs=apply(X=Grid, MARGIN=1,FUN=CalculateMAD)
	# Recall that the definition of 'CalculateMAD()' was provided earlier above (before the for-loop)

	# Next, optim() will be run on the 'top' few of these 100 first-pass candidates (i.e. those with the lowest weighted-MAD values).
	# For this purpose, the top X candidates will be used, where X is determined by the 'nSeeds' argument to this function
	# Doing so (rather than just taking the #1 best-fitting one) helps counter the fact that optim() doesn't always find the globally best solution (since it merely explores around a little bit from a given seed value)

	# Determine which of the 100 candidates have the lowest MAD values (indicating that those stylized contours fit the raw F0 data the best)
	TopCandidateIndices = order(MADs)[1:nSeeds]

	# Extract out the exact Threshold+Gradience values corresponding to those candidates, as well as their associated MAD values
	TopCandidates = Grid[TopCandidateIndices,]
	TopCandidateMADs = MADs[TopCandidateIndices]

	# Create several empty variables, the length of each being determined by 'nSeeds'
	PostOptimization_Fit <- PostOptimization_Threshold <- PostOptimization_Gradience <- rep(NA,times=nSeeds)

	# Loop through each of these top candidates (each serving as a 'seed' for the optimization process)
	for(EachTopCandidate in 1:nSeeds){

		# Extract out the Threshold and Gradience values for this 'winning'/top candidate
		WinningThreshold = as.numeric( TopCandidates[EachTopCandidate,1] )
		WinningGradience = as.numeric( TopCandidates[EachTopCandidate,2] )

		# Run R's optimization algorithm on this parameter combination to see whether (and how far) they can be tweaked to yield a better weighted MAD
		OptimalFit <- optim( par=c(WinningThreshold, WinningGradience),

		                     fn=CalculateMAD,
		                     # Again, the definition of 'CalculateMAD()' provided earlier above (before for-loop)

		                     method="L-BFGS-B",
		                     # "Limited memory - Broyden/Fletcher/Goldfarb/Shanno - Boxed", cf. footnote 9 on p.86 of Albin (2015)

		                     lower=c(Threshold=0.001,Gradience=0.001),
		                     upper=c(Threshold=0.999,Gradience=Inf)
		                     # The bounds for the search process are as described above: [0,1] for Threshold and [0,Inf] for Gradience.
		                     # Here, use three decimal places so that an initialization value (seed) of 0.01 can still explore a little lower (down to 0.001) and, likewise, a seed of 0.99 can explore a bit higher (up to 0.999).
		                     # Also note that, whereas the grid used 5 as the upper bound of Gradience, here seeds are allowed to go all the way up to Inf(inity) during the search process.

		                   ) # End 'optim()'

		# Occasionally, the solution output by optim() is worse than it started off at, so only take its output if it is indeed better
		if(OptimalFit$value < TopCandidateMADs[EachTopCandidate] ){ # If the post-optimization fit is better:
			# Read the MAD (fit), Threshold, and Gradience directly off of the OptimalFit object
			PostOptimization_Fit      [EachTopCandidate] = OptimalFit$value
			PostOptimization_Threshold[EachTopCandidate] = OptimalFit$par[1]
			PostOptimization_Gradience[EachTopCandidate] = OptimalFit$par[2]
		}else{ # If the post-optimization fit is worse
			# Revert back to the values from the original initialization seed
			PostOptimization_Fit      [EachTopCandidate] = TopCandidateMADs[EachTopCandidate]
			PostOptimization_Threshold[EachTopCandidate] = WinningThreshold
			PostOptimization_Gradience[EachTopCandidate] = WinningGradience
		} # End if/else

	} # End 'EachTopCandidate' loop

	# Determine the overall best-fitting parameter combination (pooling all [nSeeds] top candidates, both pre- and post-optimization)
	AbsoluteBest = which( PostOptimization_Fit == min(PostOptimization_Fit) )[1]
	# The [1] is added to account for the rare case that 2 or more top candidates leads to identical fit/results

	# Store the absolute/globally best combination of Threshold and Gradience in the 'Stylization' dataframe
	Stylization[EachTransition+1,"Threshold"] <- PostOptimization_Threshold[AbsoluteBest]
	Stylization[EachTransition+1,"Gradience"] <- PostOptimization_Gradience[AbsoluteBest]
	# Add +1 to the EachTransition index in order to make transitions paired up with the vertex they *end at*, not the vertex they begin with.
	# (Thus, the first vertex's values for Threshold and Gradience will always be NA.)

	# Similarly, add the weighted MAD value associated with that contour fit to the Stylization dataframe
	Stylization[EachTransition+1,"MAD"] <- PostOptimization_Fit[AbsoluteBest]

	# Finally, calculate and store the percentage of frames in this transition that were voiced (mostly for plotting purposes)
	Stylization[EachTransition+1,"PercentVoiced"] <- sum(!is.na(F0Vector))/nFrames

} # End 'each transition' for-loop

# Change the row names to reflect the fact the first row represents the starting F0 value (and not a transition per se)
rownames(Stylization) <- (1:nrow(Stylization)) - 1 

# As the output to the function as a whole, return the Stylization dataframe
return(Stylization)

########################
#   A R G U M E N T S  #
########################
# PitchPath            #
# Message=TRUE         #
# nVertices            #
# WavePath             #
# PlayDuration_ms=1000 #
# ZeroPad=TRUE         #
# ClickSound           #
# VertexIndices        #
# nSeeds=10            #
########################

} # End definition of function 'Stylize()'

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
