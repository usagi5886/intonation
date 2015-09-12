##################################
# RichVisualization()            #
# ------------------------------ #
# Copyright (C) 2015 Aaron Albin #
# http://www.aaronalbin.com/     #
##################################

# This function takes as its input a path to a soundfile and corresponding Pitch object on one's computer and produces a 'rich visualization' of the F0 data, as described on page 58-65 of:
# Albin, A. (2015). Typologizing native language influence on intonation in
#     a second language: Three transfer phenomena in Japanese EFL learners.
#     (Doctoral dissertation). Indiana University, Bloomington.
#     http://dx.doi.org/10.5967/K8JW8BSC

# This function has several dependencies:
# - Packages: audio
# - Other functions from this package ('intonation'): ReadPitch(), Spectrogram()

RichVisualization = function( # Begin argument list

# [1] Location of the files on one's computer with the acoustic data
PitchPath, # Full path to a Pitch object, stored in Praat's long 'text' format. No default.
WavePath,  # Full path to a WAV file (for creating the waveform and spectrogram). No default.

# [2] Whether to make a new window or inherit an existing one
SameWindow = FALSE, # Defaults to creating a new window
# Note that, if 'SameWindow' is set to TRUE, it is assumed that the inherited previous window was also a RichVisualization (with its unique settings for margins, number of panels, etc.)

# [3] Window size
# If 'SameWindow' is FALSE, specifies the dimensions of the new window to be created
Width  = 6.5,  # 6.5 inches, which fills up the horizontal space on a 8.5x11 paper with 1-inch margins.
Height = 4.33, # 4.33 inches, i.e. (roughly) two-thirds the width (i.e. 6.5*2/3)
# Note: If 'SameWindow' is set to TRUE, then the values for Width and Height are ignored (with a warning)

# [4] x axis limits
xlim, # The time range to be plotted (for all three panels in the plot: waveform, F0 track, and spectrogram)
# If left unspecified, the full time range of the soundfile will be used.
# Note that, even if 'xlim' is specified, R will still process the full time range of the soundfile, hence the processing time will be slow on long soundfiles.

# [5] F0 range
F0Range, # The F0 range (in Hertz) for the F0 track panel of the plot
# If left unspecified, the floor will be the lowest (first-candidate) F0 point stored in the Pitch object, and the ceiling will be taken from the header of the Pitch object.

# [6] Manual segmentation
Labels, # A character vector (of any length) containing the label(s) to go in the boxes of the spectrogram pane, representing some linguistic unit (segments, syllables, words, etc.)
Divisions_ms, # A numeric vector with the timestamps (in milliseconds) of the vertical bars to be drawn between the segments/syllables/words/etc.
# 'Divisions_ms' should be length(Labels)+1 components long and in strictly increasing/ascending order.
# The first and/or last values inside 'Divisions_ms' can be made to match up to the range of time specified for 'xlim' by specifying them as NA. For example, if xlim=c(1,5), then Divisions_ms=c(NA,2,3,4,NA) is equivalent to Divisions_ms=c(1,2,3,4,5).
# If 'Labels' and/or 'Divisions_ms' is left unspecified, no labels or divisions will be drawn (hence the spectrogram pane will contain just the spectrogram alone, without any superimposed lines or text).

# [7] TextGrid segmentation
TextGridPath, # Path to a TextGrid on one's computer from which the segmentation should be drawn. No default.
TextGridTier, # The name of an interval tier containing the segmentation boundaries and labels, e.g. "words". (Does nothing if 'TextGridPath' is left unspecified.)
# If 'TextGridPath' is provided, it will override any specification of the 'Labels' and 'Divisions_ms' arguments (with a warning).

# [8] Silence label
SilenceLabel, # Some 'silence label' (e.g. "<SIL>") that should be erased (i.e. changed to "") when plotting the segmentation
# Corpus annotations often have a symbol specifically for explicitly marking silence.
# However, when viewing a plot, the fact a given interval represents silence is clear enough if that interval is simply left empty.
# Indeed, leaving the silence labels in can clutter up the segmentation, especially if the silent intervals are short.
# Thus, this argument 'SilenceLabel' can be set to any character string (e.g. "<SIL>") to have all instances of that label changed to "" (an empty string) in the segmentation.
# Since there is no default value for this argument, the silence label is transparently printed as-is unless the user requests otherwise.

# [9] On/off switch for making the F0 track grayscale
Grayscale = FALSE, # Defaults to FALSE (full color)
# Note the spelling is 'Grayscale' (with an 'a', not an 'e')

# [10] Font size # (i.e. 'character expension', or 'cex') for the text labels in the spectrogram
Spectrogram_FontSize = 1.5, # 150% default font size (to make the labels easy to read)

# [11] y axis limits for the spectrogram
Spectrogram_ylim, # A vector of length 2, with the minimum and maximum frequency to be plotted
# If left unspecified, it will be with set to {min=0, max=that soundfile's Nyquist frequency}

# [12] Color palette for the spectrogram
Spectrogram_col=c("white","grey75"), # Defaults to a faded grayscale (to ensure the labels are can be easily read)
# See documentation for 'col' argument of Spectrogram() for how to specify other palettes.

# [13] Any other arguments are passed to Spectrogram()
... # Arguments that are commonly tweaked include WindowLength, FrequencyResolution, nTimeSteps, and DynamicRange.

){ # End argument list; Begin body of actual function

# Require audio package (for load.wave() function down below)
require("audio")

####################
# Input validation #
####################

# Make sure the user didn't forget to provide a PitchPath or WavePath
if( missing(PitchPath) ){ stop("Must specify PitchPath (path to Pitch object).") }
if( missing(WavePath) ){ stop("Must specify WavePath (path to WAV file).") }

# Make sure the file paths for 'PitchPath' and 'WavePath' are valid (i.e. some file actually exists at that each of the two paths)
if( !file.exists(PitchPath) ){ stop("No file found at specified PitchPath.") }
if( !file.exists(WavePath) ){ stop("No file found at specified WavePath.") }

# If the user indicates they want to inherit an existing window but nonetheless specify some non-default height and/or width, issue a warning.
if( SameWindow==TRUE & ( (Width!=6.5) | (Height!=4.33)) ){ warning("The specified values for Width and/or Height have been ignored.\n(These should be omitted if SameWindow=TRUE)")}

# Check whether the user provided a manual segmentation (via the two arguments 'Labels' and 'Divisions_ms'))
SegmentationProvided = !missing(Labels) & !missing(Divisions_ms) # Neither is missing, hence all relevant information is fully provided

# If the user provided a manual segmentation, make sure the divisions and labels look all right:
if(SegmentationProvided){
 # Make sure the number of labels and divisions match up
 if( length(Divisions_ms)-1 != length(Labels) ){stop("The numbers of divisions and labels do not match up.\n       length(Divisions_ms) should be length(Labels)+1")}
 # Make sure the divisions are sorted (in ascending/increasing order)
 if( !identical( Divisions_ms[!is.na(Divisions_ms)], sort(Divisions_ms) ) ){"The divisions must be in strictly ascending/increasing order."}
}# End 'if segmentation is provided'

# If the user provides a TextGridPath but nonetheless also provides a manual segmentation, issue a warning
if( SegmentationProvided & !missing(TextGridPath) ){ warning("Supplied 'Labels' and/or 'Divisions_ms' arguments ignored; supplied 'TextGridPath' used instead.") }

# If the user provides a TextGridPath
if( !missing(TextGridPath)){

	# Make sure it points to an actual file.
	if( !file.exists(TextGridPath) ){ stop("No file exists at the specified TextGridPath.") }
	# Note that it is left assumed (i.e. not explicitly verified) that the file at this path is indeed a Praat TextGrid in (long) 'text' format.

	if( missing(TextGridTier) ){
		stop("Must specify TextGridTier (name of tier with the segmentation).")
	} # End 'if TextGridTier is missing'

	# Now read in the contents of the file
	TextGrid_FullText = readLines(TextGridPath)

	# Find which line in the file has the name of the desired tier (e.g. 'words')
	Pattern = paste("name = \"",TextGridTier,"\"",sep="")
	TierNameIndex = grep(TextGrid_FullText,pattern=Pattern,fixed=TRUE)

	# If the specified tier name is not found in the TextGrid, issue an error
	if(length(TierNameIndex)==0){ stop(paste("There is no tier named '",TextGridTier,"' in the specified TextGrid.",sep="")) }

	# Note that several of the variables just created will stay in the workspace to be drawn upon later down below in the script

} # End 'if the user provides a TextGridPath'

########################################
# Import and process basic information #
########################################

# Use ReadPitch() (another function in the package) to import all 3 kinds of information about the Pitch object
Header = ReadPitch( PitchPath, Return="Header" ) # Needed to determine the ceiling (if F0Range is not provided)
Frames = ReadPitch( PitchPath, Return="Frames" )
Candidates = ReadPitch( PitchPath, Return="Candidates" )

# Extract out the time information
Time_s = Frames$Time
Time_ms = 1000*Time_s

if(missing(xlim)){ # If 'xlim' (the desired range of time to be plotted in all three panels) is left unspecified
	xlim=c(0,max(Time_ms)) # use the full time range by default
} # End 'if xlim is missing'


# #########################
# Set up the overall plot #
# #########################

# The RichVisualization plot has these three panes, which can be represented schematically as follows:
# [  Waveform   ]
# [  F0 track   ]
# [ Spectrogram ]

# Of these, the F0 track is vertically taller than the other two panels.

# To save space on the page, it may be occasionally be desirable to omit one or more of these (e.g. the waveform).
# However, this function (as currently implemented) creates all three as one inseparable 'bundle'.

# If needed, create a new window
if(SameWindow==FALSE){ # i.e. if it's not the case that some pre-existing window should be inherited
	dev.new(width=Width, height=Height)
} # End 'SameWindow is FALSE'

# Set the 'global' graphical parameters, i.e. those that won't change for the entirety of this function (RichVisualization)
par( cex.axis=1.25, # Make the character expansion of the axis numbering (in all three panels) slightly larger than normal
	las=1, # Make the numbering along the y axis of the spectrogram and F0 track display horizontally (left to right), hence potentially easier to read
	mgp=c(AxisTitle=2.5, AxisLabels=0.6, AxisLine=0) # Margin parameters, tweaked to minimize unused white space
) # End 'par()'

# The overall flow of the rest of this function is to first draw the spectrogram+segmentation (bottom), then the waveform (top), and then finally the F0 track (middle).
# Since the F0 track is drawn last, the ultimate coordinate system is always defined by the F0 track.
# This is essential for adding annotations (e.g. the points/text/lines in a stylization) to the F0 track after the rich visualization is drawn.
layout(
	matrix(c(2,3,1),nrow=3,ncol=1), # Specify drawing order (2,3,1)
	heights=c(0.5,2,0.75) # Compared to the F0 track, The waveform panel is 25% as tall and the spectrogram panel is 37.5% as tall.
) # End 'layout()'


#################################################
# Create the bottom panel (for the spectrogram) #
#################################################

# Set margins for the spectrogram plot
par(mai=c(0.4, 0.5, 0.1, 0.2))

# Load in the wave file
WaveFile=audio:::load.wave(WavePath) # From package 'audio'

# Determine the wave file's sampling frequency
SamplingFrequency = attributes(WaveFile)$rate

# If the user did not specify Spectrogram_ylim (y-axis limits for the spectrogram), fill in the default (0 to the Nyquist frequency)
if(missing(Spectrogram_ylim)){ Spectrogram_ylim=c(0,SamplingFrequency/2) }

# Draw the spectrogram itself using the Spectrogram() function (also bundled in this package)
par(yaxt="n") # Turn plotting of the y axis off temporarily
Spectrogram( Audio=WaveFile, xlim=xlim, ylim=Spectrogram_ylim, main="", xlab="", ylab="", col=Spectrogram_col, ...) # Any other arguments from RichVisualization() are passed to Spectrogram() here
par(yaxt="s") # Turn plotting of the y axis back on

# Add the x-axis label
mtext(side=1,text="Time (ms)",line=1.75, cex=0.85)

# Add the y-axis numbering (at just the endpoints of the axis)
axis(side=2,at=Spectrogram_ylim)

#------------------------------------------------

# If the user specifies something for TextGridPath, process the information therein in order to generate the missing 'Labels' and 'Divisions_ms' variables.
if( !missing(TextGridPath)){

# Within the TextGrid file, the core lines of interest look something like this:
#    item [2]:
#        class = "IntervalTier" 
#        name = "words" 
#        xmin = 0 
#        xmax = 4.52952380952381 
#        intervals: size = 13 
#        intervals [1]:
#            xmin = 0 
#            xmax = 0.60780053099966969 
#            text = "<SIL>" 
#        intervals [2]:
#            xmin = 0.60780053099966969 
#            xmax = 0.75678999999999996 
#            text = "what" 
#		[...]
#    item [3]:

# Pull out the line numbers of all 'item [_]' lines (i.e. tiers in the TextGrid)
ItemHeaders = grep(TextGrid_FullText,pattern="    item [",fixed=TRUE)

# Determine how many 'items' (tiers) there are in total
nItems = length(ItemHeaders)

# Use this information to determine the beginning and ending of the regions in the textfile for each item/tier
ItemDivisionPoints = c(ItemHeaders,length(TextGrid_FullText)-1)
BeginningIndices = ItemDivisionPoints[1:nItems]
EndingIndices = ItemDivisionPoints[2:(nItems+1)]

# Determine which tier is the target one (specified in 'TextGridTier')
WhichItem = ( TierNameIndex > BeginningIndices ) & ( TierNameIndex < EndingIndices )

# Find the range of lines in the textfile associated with that tier
TargetBeginning = BeginningIndices[WhichItem]
TargetEnding = EndingIndices[WhichItem]

# Find all lines in the textfile containing 'text = ' (i.e. a label in an interval tier)
TextLocations = grep(TextGrid_FullText,pattern="text = ",fixed=TRUE)

# In case there are multiple interval tiers, filter this down to the ones that occur in the target tier
FilteredTextLocations = TextLocations[ (TextLocations > TargetBeginning) &  (TextLocations < TargetEnding) ]

# Determine the line numbers with the left and right edges of the intervals in this tier
xminIndices = FilteredTextLocations - 2
xmaxIndices = FilteredTextLocations - 1

# Extract out the full text in each of these lines
xmins_Character  = TextGrid_FullText[xminIndices]
xmaxes_Character = TextGrid_FullText[xmaxIndices]

# Filter this down to just the numeric information (i.e. the timestamps)
xmins  = as.numeric( substring(xmins_Character, first=20,last=nchar(xmins_Character )-1) )
xmaxes = as.numeric( substring(xmaxes_Character,first=20,last=nchar(xmaxes_Character)-1) )

Divisions_ms = 1000 * c( xmins[1], xmaxes )

TextLines = TextGrid_FullText[FilteredTextLocations]
Labels = substring(TextLines, first=21,last=nchar(TextLines)-2)

} # End 'if TextGridPath is provided'

#------------------------------------------------

# Now draw on the 'Labels' and 'Divisions_ms' variables (regardless of whether they were manually specified in the function call or extracted from the TextGrid) to plot text and lines
if(SegmentationProvided | !missing(TextGridPath) ){

# Change all labels explicitly marking silence (e.g. "<SIL>") to "", as requested by the 'DeleteSIL' argument
if( !missing(SilenceLabel) ){ Labels[Labels==SilenceLabel] <- "" }

# Add the vertical lines at the division locations
abline(v=Divisions_ms)

# Next, add the labels inside the segmentation.
# (The input validation has ensured that divisions and labels match up in terms of their lengths.)
nDivisions = length(Divisions_ms)

# Using the 'Divisions_ms' variable, make a vector for the calculation of where the labels should be horizontally located (in the midpoints of the corresponding intervals).
Divisions_Calculation <- Divisions_ms

# Fill in the first and last NAs of this calculation vector with the xlim values.
# (The xlim is either (1) specified by the user in the function call or else (2) was filled in by the default earlier up above in this script.)
if( is.na(Divisions_ms[1])          ){Divisions_Calculation[1] <- xlim[1]}
if( is.na(Divisions_ms[nDivisions]) ){Divisions_Calculation[nDivisions] <- xlim[2]}

# Horizontal alignment of the labels: Calculate the midpoint locations
Labels_X = ( Divisions_Calculation[1:(nDivisions-1)] + Divisions_Calculation[2:nDivisions] )/2

# Vertical alignment of the labels:
if(missing(Spectrogram_ylim)){ # If Spectrogram_ylim is left unspecified, use half the Nuquist frequency, which is 1/4 the sampling rate
Labels_Y = SamplingFrequency/4
}else{ # Otherwise just use the midpoint of the Spectrogram_ylim
Labels_Y = mean(Spectrogram_ylim)
} # End if/else

# Finally, actually add the labels to the plot
text( x=Labels_X, y=Labels_Y, labels=Labels,cex=Spectrogram_FontSize, adj=c(0.5,0.5) )
# Note: Manually specifying 'adj' ensures no vertical misalignment from presence vs. absence of letters like 'g' and 'y')

} # End 'if segmentation is provided


#####################
# Make the waveform #
#####################

# Set the margins for the waveform pane of the plot
par(mai=c(0, 0.5, 0.1, 0.2))

# Create the appropriate time-series (with milliseconds as its units)
Waveform_TimeSeries = 1000* ( 1:length(WaveFile)/SamplingFrequency )

# Actually make the waveform plot
plot(x=Waveform_TimeSeries, y=WaveFile, main="", type="l", xaxt="n", xlab="", yaxt="n", ylab="", xaxs="i", xlim=xlim )

# Add the vertical lines for the divisions (if any)
if(SegmentationProvided | !missing(TextGridPath) ){ abline(v=Divisions_ms) }


##########################################
# Make the main plot (with the F0 track) #
##########################################

# First, map strength onto color
# Since most of the interesting 'action' occurs between 0.5 and 1.0, the code below makes the color scale highlight things in this range.

# Shift the distribution of raw strength values from the signal by -0.5 so it tops out at 0.5
Strength_Intermediate = Frames$Strength - 0.5

# Change any negative value to 0, i.e. 'snap' the lower tail of the distribution to 0.
# This mostly just affects unvoiced frames.
# Any frame that is 0 will be exactly the color indigo.
Strength_Intermediate[Strength_Intermediate < 0 ] <- 0

# Rescale the distribution from [0, 0.5] to [0, 1] by multiplying by 2
NormalizedStrength = 2*Strength_Intermediate

if(Grayscale){ # Depending on the setting of the 'Grayscale' argument to the function (Defaults to 'Grayscale=FALSE')

ColorGenerator = colorRampPalette( c("gray95","gray50") )
# Stop at gray95 so you can still (barely) see faint points on white.
# Stop at gray50 so the black stylization lines are visible.

}else{ # i.e. if producing a full-color plot

ColorGenerator = colorRampPalette(rev(c("red", "orange", "yellow", "green", "blue", "#4B0082")))
# Note that #4B0082" = Indigo.
# Later below, violet is used for secondary points (if Grayscale=FALSE).
# Thus, the color palette is ROYGBIV.
# Note that, in the current implementation of the RichVisualization() function, this color palette is built into the function (and cannot be toggled to something else by the user, e.g. by specifying a 'col' argument),

} # End if/else Grayscale'

# Make 256 colors based off of the relevant color-generating function and map strength values accordingly (1 to 256)
Colors = ColorGenerator(256)[round(255*NormalizedStrength)+1]

#-----------------------------------------

# Determine the F0Range if it is not explicitly specified in the call to RichVisualization()
if(missing(F0Range)){
	Floor = round(min(Candidates$Frequency,na.rm=TRUE)/5)*5 # The lowest of *all* candidates (not just first-ranked ones), rounded (up *or* down) to the nearest 5
	Ceiling = Header$Ceiling # Lifted directly from the header of the Pitch object
	F0Range = c(Floor, Ceiling)
} # End 'if F0Range is missing'

# Set the margins for the main (F0 track) panel of the plot
par(mai=c(0, 0.5, 0.1, 0.2))

# Plot the primary points
plot(x=Time_ms, y=Frames$Frequency, cex=Frames$Intensity*1.5, pch=16,col=Colors,las=1, xaxs="i", xlab="", ylab="", xlim=xlim, ylim=F0Range, xaxt="n") # main=FileName

# Add the y axis title
mtext(side=2,text="F0 (Hz)",line=2.5,las=0, cex=0.85)

#-----------------------------------------

# Now handle the secondary candidates

# There are four logically possible scenarios:
# 1) First=voiced,    second=voiceless   # Plot the first-ranked candidate
# 2) First=voiced,    second=voiced      # Plot the first-ranked candidate
# 3) First=voiceless, second=nonexistent # Plot nothing
# 4) First=voiceless, second=voiced      # Plot the second-ranked candidate as captured in the code below

# Thus, this code only needs to worry about scenario 4
# To do the plotting, draw on the rows in the 'Candidates' matrix (created at the beginning of this function, under 'Import and process basic information')

# Find the set of all frames whose first-ranked candidate is voiceless
FirstVoiceless = Candidates[Candidates$nthCandidate==1 &  is.na(Candidates$Frequency), "nthFrame"]

# Find the set of all frames whose second-ranked candidate (exists and) is voiced
SecondVoiced = Candidates[Candidates$nthCandidate==2 & !is.na(Candidates$Frequency), "nthFrame"]

# The 'target' frames are the intersection of the above two sets
TargetFrames = intersect(FirstVoiceless,SecondVoiced)

# For the kinds of frames just identified, pull out just the second candidates (since those are the ones to be plotted)
TargetRowFilter = ( Candidates$nthFrame %in% TargetFrames ) & ( Candidates$nthCandidate==2 )

# Collate various pieces of information about these secondary candidates
Secondary_nthFrame = Candidates$nthFrame[ TargetRowFilter ] # Which frame this candidate is in
SecondaryF0 = Candidates$Frequency[ TargetRowFilter ] # F0 (in Hertz)
SecondaryStrength = Candidates$Strength[ TargetRowFilter ] # Raw (i.e. unnormalized) strength
SecondaryIntensity = Frames$Intensity[Secondary_nthFrame] # Intensity
SecondaryTime_ms = Time_ms[Secondary_nthFrame] # Time (in milliseconds)

if(Grayscale){

	# Actually add the points to the plot
	points(SecondaryTime_ms,SecondaryF0,pch=".",cex=1.5)
	# All the same color (black) and all the same size (cex=1.5 - big enough for it to be clear that the points are black)

}else{ # i.e. if producing a full-color plot

	# Map [0,1] onto [white,violet]
	# (No need to re-scale from [0.5,1] first, as was done above for first-ranked candidates.)
	VioletScale = colorRampPalette(c("white","violet"))
	SecondaryColors = VioletScale(256)[round(255*SecondaryStrength)+1]

	# Actually add the points to the plot
	points(SecondaryTime_ms,SecondaryF0,pch=16,cex=SecondaryIntensity*1.5,col=SecondaryColors)

} # End if/else

# Add the vertical lines for the divisions to this panel of the plot (the F0 track) as well
if(SegmentationProvided | !missing(TextGridPath) ){ abline(v=Divisions_ms) }

########################################
#           A R G U M E N T S          #
########################################
# PitchPath                            #
# WavePath                             #
# SameWindow = FALSE                   #
# Width = 6.5                          #
# Height = 4.33                        #
# xlim                                 #
# F0Range                              #
# Labels                               #
# Divisions_ms                         #
# TextGridPath                         #
# TextGridTier                         #
# SilenceLabel                         #
# Grayscale = FALSE                    #
# Spectrogram_FontSize = 1.5           #
# Spectrogram_ylim                     #
# Spectrogram_col=c("white","grey75")  #
# ... # Passed to Spectrogram()        #
########################################

} # End definition of function 'RichVisualization()'

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
