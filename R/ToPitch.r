##################################
# ToPitch()                      #
# ------------------------------ #
# Copyright (C) 2015 Aaron Albin #
# http://www.aaronalbin.com/     #
##################################

# Takes a soundfile and produces a Pitch object using the specified input parameters

# This function is referenced on page 55 of:
# Albin, A. (2015). Typologizing native language influence on intonation in
#     a second language: Three transfer phenomena in Japanese EFL learners.
#     (Doctoral dissertation). Indiana University, Bloomington.
#     http://dx.doi.org/10.5967/K8JW8BSC

# _________________________________________________________________________
# _________________________________________________________________________
# _________________________________________________________________________

# Praat has five different commands that extract F0:
# [1] To Pitch...
# [2] To Pitch (ac)...
# [3] To Pitch (cc)...
# [4] To Pitch (SPINET)...
# [5] To Pitch (shs)...

# Of these, [1] is just a high-level convenience wrapper/interface to [2].

# The three arguments to "Sound: To Pitch", along with their defaults, are as follows
# Time step (s):        0.0 (= auto)
# Pitch floor (Hz):    75.0
# Pitch ceiling (Hz): 600.0

# If you use "To Pitch (ac)...", you unlock all of these advanced settings:
# (See also Table 3.2 on p56 of Albin (2015).)

# Finding the candidates
#   Time step (s):              0.0 (= auto)
#   Pitch floor (Hz):          75.0
#   Max. number of candidates: 15
#   [+/-] Very accurate        (In PraatR, "no" or "yes")
# Finding a path:
#   Silence threshold:         0.03
#   Voicing threshold:         0.45
#   Octave cost:               0.01
#   Octave-jump cost:          0.35
#   Voiced / unvoiced cost:    0.14
#   Pitch ceiling (Hz):      600.0  # NOTE: The default is 500 in the editor window.

# In order to maximum flexibility by unlocking all of these advance settings, this function uses "To Pitch (ac)...".
# The decrement in speed relative to "To Pitch..." is probably negligible.

ToPitch = function( # Begin argument list

# [1-2] Input and output
Input, # Path to a WAV file on one's computer
Output, # Path to the Pitch object textfile that will be created as output
# Neither of these arguments have defaults.
# These must both be full file paths (i.e., you can't rely on R's working directory functionality)
# The file paths also must contain no spaces (due to a limitation in the current implementation of PraatR).
# Note that, unlike the 'PraatR' package, both of these argument names are capitalized ('[I]nput' and '[O]utput')

# [3] Overwrite
Overwrite=FALSE, # If the 'Output' file already exists on your computer, determines whether it will be overwritten.
# To be on the safe side, the default is left at FALSE (following PraatR), whereby an error message is issued.
# Here again, this argument name is capitalized ('[O]verwrite'), unlike in PraatR.
# This is passed directly to the 'praat()' function.

# [4] FileType
FileType = "text", # What kind of format the Pitch object data should be stored in
# The options are 'text', 'short text' (='short'), or 'binary'.
# 'binary' saves the most disk space, whereas 'text' (the default) is more convenient for processing in R.

# [5] Range
Range, # The F0 range (in Hertz) for the F0 analysis, specified as a vector
# Either c(ceiling, floor) or c(floor, ceiling) will work, but the latter is perhaps more conventional.
# In Praat, the default floor is 75 and the default ceiling is either 500 (objects window) or 600 (editor window).
# In this function, the 'Range' argument does *not* have a default value, though, since it is good practice to specify the range on a file-by-file basis.
# (Even if the same value is used for many files, specifying it makes one's assumptions explicit.)

# [6] Time step
TimeStep=0.001, # Size of the time step (or 'frame') for the analysis, in milliseconds
# The default of 0.001 means an F0 value will be extracted at every millisecond.
# Note that this is *not* the Praat default, which is '0' (auto).

# [7] Maximum number of candidates
MaxNumberOfCandidates=15, # The maximum number of candidate F0 values in each frame
# The default value here (15) follows Praat's default.
# Setting this to 2 will save disk space and will not affect many (if not most) analyses.

# [8] Advanced parameters
# All of these are left at their defaults and therefore normally do not need to be specified.
# When they do need to be specified, they can just be included in the call to ToPitch() as needed.
VeryAccurate=FALSE, # Note: 'FALSE' (logical) is interchangeable with "no" (character) in PraatR.
SilenceThreshold=0.03,
VoicingThreshold=0.45,
OctaveCost=0.01,
OctaveJumpCost=0.35,
VoicedUnvoicedCost=0.14

){ # End argument list, and begin body of function definition

# Require the PraatR package
require("PraatR")

# Check to make sure none of the required arguments are missing:
MissingCheck = missing(Input) | missing(Output) | missing(Range)
if( MissingCheck ){
	MissingList = paste( c("Input", "Output", "Range")[ c( missing(Input), missing(Output), missing(Range) ) ], collapse=", " )
	stop(paste("You need to specify the following arguments: ",MissingList))
} # End 'if any necessary arguments are missing'

# Do some basic input validation to ensure Range is a numeric (or integer) vector of length 2
if(!( mode(Range) %in% c("numeric","integer") )){ stop("'Range' must be a numeric or integer vector.") }
if(length(Range)!=2){ stop("'Range' must be of length 2, e.g. c(floor,ceiling)") }
FinalRange = sort(Range) # to ensure the F0 range is in the standard format c(floor, ceiling)

# Assemble all of the arguments into a list
Arguments=list(TimeStep=TimeStep,
               PitchFloor=FinalRange[1],
               MaxNumberOfCandidates=MaxNumberOfCandidates,
               VeryAccurate=VeryAccurate,
               SilenceThreshold=SilenceThreshold,
               VoicingThreshold=VoicingThreshold,
               OctaveCost=OctaveCost,
               OctaveJumpCost=OctaveJumpCost,
               VoicedUnvoicedCost=VoicedUnvoicedCost,
               PitchCeiling=FinalRange[2])
# Note that the ordering in this list follows Praat's order of arguments.
# This is different from the default order of arguments to this function (ToPitch()) as a whole.

# Perform the actual PraatR command
# This requires PraatR to be loaded, but it is a package dependency for the 'intonation' package as a whole
PraatR:::praat( command="To Pitch (ac)...",
                arguments=Arguments,
                input=Input,
                output=Output,
                overwrite=Overwrite,
                filetype=FileType
) # End call to 'praat()'

############################
#    A R G U M E N T S     #
############################
# Input                    #
# Output                   #
# Overwrite=FALSE          #
# FileType="text"          #
# Range                    #
# TimeStep=0.001           #
# MaxNumberOfCandidates=15 #
# VeryAccurate=FALSE       #
# SilenceThreshold=0.03    #
# VoicingThreshold=0.45    #
# OctaveCost=0.01          #
# OctaveJumpCost=0.35      #
# VoicedUnvoicedCost=0.14  #
############################

} # End definition of function 'ToPitch()'

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
