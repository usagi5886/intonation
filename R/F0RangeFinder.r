##################################
# F0RangeFinder()                #
# ------------------------------ #
# Copyright (C) 2016 Aaron Albin #
# http://www.aaronalbin.com/     #
##################################

# This function cycles through each soundfile in the specified 'WaveFolder' and helps you quickly determine an optimal F0 range (i.e. 'floor' and 'ceiling' values) for each in a semi-automatic fashion.

# This function is not directly referenced in, but was used for, the following dissertation:
# Albin, A. (2015). Typologizing native language influence on intonation in
#     a second language: Three transfer phenomena in Japanese EFL learners.
#     (Doctoral dissertation). Indiana University, Bloomington.
#     http://dx.doi.org/10.5967/K8JW8BSC

F0RangeFinder=function(

# [1]
WaveFolder, # Path to the folder on your computer that contains the soundfiles to be processed.
# May not contain any spaces, and must end in either a slash ('/') or a backslash (\\').
# Each soundfile therein must end in either a .wav or .WAV file extension.

# [2]
PitchFolder,
# Path to the folder on your computer where you would like to save the various pitch-related files that are created as a byproduct of using this function.
# May not contain any spaces, and must end in either a slash ('/') or a backslash (\\').
# Note:
# - Input validation confirms that this folder exists, and the 'Overwrite' argument below controls over-writing.
# - Besides this, no other checks are performed on the contents of this folder.
# - Thus, it could in principle contain various other files (e.g. one's desktop), though this is not recommended.

# [3]
StartAt=1, # Which soundfile to start processing from
# Can be specified either as an index (e.g. '5' for the 5th file) or as a filename (e.g. 'MySound.wav').
# Defaults to '1', hence it will start at the first file and work through all of them.

# [4]
Output="object", # i.e. a Pitch object
# The storage format of the pitch-related files that are created as a byproduct of using this function.
# Can be set to values in three different formats: "object"/"tier"/"listing" "o"/"t"/"l", or 1/2/3, as follows:
# - 1/object/o: for a Praat Pitch object, saved in (long) text format
# - 2/tier/t: for a Praat PitchTier object, saved in (long) text format
# - 3/listing/l: for a format similar to a Pitch Listing (normally obtained from the editor window) except that it lacks headers and lacks the '--undefined--' placeholders for voiceless frames.

# [5]
OutputExtension=NULL,
# Determines the file extension of the output file.
# Must begin with a period. By default (i.e. if OutputExtension is left NULL, it depends on the Output argument. If "object" (or equivalent), it is .Pitch. If "tier", it is .PitchTier. If "listing", it is '.txt'.
# Note that OutputExtension will be applied regardless if any given input file is .wav or .WAV (i.e. lower-case or capital).

# [6]
Overwrite=FALSE, # If any files already exist in PitchFolder, defines whether over-writing should occur.
# Defaults to \code{FALSE}, thus protecting against accidental loss of data.
# Note that this argument is capitalized - [O]verwrite - unlike the lower-case 'overwrite' argument of praat() from the PraatR package.
# This comes into play on two occasions:
# - before cyclical looping for each file (affecting the creation of the .Pitch file)
# - after any given file is done, if it is to be converted into PitchTier or pitch listing format (in which case it affects the creation of the latter two kinds of file)
# Besides the above two checks, this does not affect the cyclical trial-and-error looping process whereby various F0 ranges are applied to a single file. (Over-writing is enabled at that stage of the code out of necessity.)

# [7]
StartingRange=c(74.99,500.01), # A numeric (or integer) vector of length 2 specifying the F0 range to be used as the first approximation for every file.
# Defaults to c(74.99, 500.01), i.e. effectively [75, 500] except the values are bumped by +/-0.01 as an explicit marker that these are the initial values (and not an ultimately-decided F0 range).

# [8]
Next="z", # The text to type to move to the next file.
# Defaults to "z" (since this key is on the corner of the keyboard and close to the 's' key for 'Snap'.

# [9]
Play="p", # The text to type to play the soundfile.
# Defaults to "p" (the first letter of the word 'play').

# [10]
Snap="s", # The text to type to 'snap' the F0 range to the range defined by the points in the current plot (i.e. the minimum and maximum F0 values of the observed points).
# Defaults to "s" (the first letter of the word 'snap').

# [11]
Quit="q", # The text to type to quit the analysis at any time (e.g. to take a break).
# Defaults to "q" (the first letter of the word 'quit').

# [12]
Delete=FALSE, # Whether the .Pitch objects created as a byproduct of using this function should be deleted. Useful to clean up unnecessary clutter in PitchFolder, especially if Output is set to "tier" or "listing".
# Defaults to FALSE, thus conservatively preserving all data created as part of the analysis.
# In particular, setting Delete=TRUE can be useful in two instances:
#- If creating Pitch Objects, Delete=TRUE will let you get just the F0 ranges but remove all trace of your determining those ranges (in the terms of files on your hard drive).
#- If creating pitch tiers or pitch listings, Delete=TRUE allows you to free up hard drive space by not retaining the intermediate Pitch object representations.

# [13]
Dataframe, # A dataframe created from a previous run of this function.
# Useful for picking up where one left off, e.g. after taking a break.

# [14]
Width=10, # Width of the window to be created.

# [15]
Height=5 # Height of the window to be created.

){ # End arguments list; Begin body of function

####################
# Input validation #
####################

#............
#.WaveFolder.
#............

# Make sure the WaveFolder exists
if( !dir.exists(WaveFolder) ){ stop("No directory exists at the path specified for 'WaveFolder'.") }

# Make sure the WaveFolder path does not contain a space
if( " " %in% strsplit(WaveFolder,split="")[[1]] ){ stop("The path provided for 'WaveFolder' cannot contain a space.") }

# Make sure the WaveFolder path ends in a slash or backslash
nCharacters_WaveFolder = nchar(WaveFolder)
LastCharacter_WaveFolder = substring(WaveFolder,first=nCharacters_WaveFolder,last=nCharacters_WaveFolder)
if( !( LastCharacter_WaveFolder %in% c("/","\\") ) ){stop("'WaveFolder' must end in either a slash ('/') or backslash ('\\').")}

# Make a list of all the wave files in this folder
FileList = list.files(WaveFolder)

# Count how many files we're dealing with
nFiles = length(FileList)

# Make sure everything is a wave file
# Determine this by making sure all filenames end in '.wav' or '.WAV'
# (These two possibilities - '.wav' and '.WAV' - can be intermixed at will.)
nCharacters_FileList = nchar(FileList)
AllExtensions = substring( FileList, first=nCharacters_FileList-3, last=nCharacters_FileList )
WaveExtensionFound = ( AllExtensions == ".wav" ) | ( AllExtensions == ".WAV" )
PerfectMatch = sum(WaveExtensionFound) == nFiles # The number of files ending in '.wav' or '.WAV' should equal nFiles.
if(!PerfectMatch){ # i.e. if there is one or more non-wave files in WaveFolder
NonWaveFileIndices = which(!WaveExtensionFound)
stop(paste("The following files in 'WaveFolder' do not end in '.wav' or '.WAV':\n       ",paste(FileList[NonWaveFileIndices],collapse="\n       "),sep=""))
} # End 'if there is not a perfect match (between the list of files and the files that end in '.wav')

#.............
#.PitchFolder.
#.............

# Make sure the PitchFolder exists
if( !dir.exists(PitchFolder) ){ stop("No directory exists at the path specified for 'PitchFolder'.") }

# Make sure the PitchFolder path does not contain a space
if( " " %in% strsplit(PitchFolder,split="")[[1]] ){ stop("The path provided for 'PitchFolder' cannot contain a space.") }

# Make sure the PitchFolder path ends in a slash or backslash
nCharacters_PitchFolder = nchar(PitchFolder)
LastCharacter_PitchFolder = substring(PitchFolder,first=nCharacters_PitchFolder,last=nCharacters_PitchFolder)
if( !( LastCharacter_PitchFolder %in% c("/","\\") ) ){stop("'PitchFolder' must end in either a slash ('/') or backslash ('\\').")}

#.........
#.StartAt.
#.........

# Check whether StartAt is acceptable
if(class(StartAt) %in% c("numeric","integer") ){ # If it's a numeric index:
	# Make sure it falls within the range of [1:nFiles]
	if( !(StartAt %in% 1:nFiles) ){ stop(paste("Since there are ",nFiles," files in WaveFolder,\n       'StartAt' must be an integer between 1 and ",nFiles,".",sep="")) }
	StartingIndex <- StartAt # Store the StartAt index inside a new variable.
	# If 'StartAt' is left at the default, this results in 'StartingIndex=1'.
}else{
	if(class(StartAt) == "character" ){ # If it's a filename:
		# Make sure it's one of the filenames included in the 'FileList' vector
		if( !( StartAt %in% FileList ) ){ stop("There is no file in the 'WaveFolder' directory\n       under the filename provided for the 'StartAt' argument.") }
		StartingIndex = which( FileList==StartAt ) # This should return 1 (and only 1) index
	}else{ # Otherwise, 'StartAt' is of an unacceptable class, hence issue an error message.
		stop("Argument 'StartAt' must be of class numeric, integer, or character.")
	} # End if/else StartAt' is of class 'character'
}# End if/else StartAt is of class 'numeric' or 'integer'

#........
#.Output.
#........

# Make sure the output file format is recognized
if( !( Output %in% 1:3 ) &
    !( Output %in% c("o","t","l") ) &
    !( Output %in% c("object","tier","listing") ) ){stop("The 'Output' argument must be one of the following:
       1 / o / object
       2 / t / tier
       3 / l / listing")}

#.................
#.OutputExtension.
#.................

# Process what was provided for OutputExtension
if( is.null( OutputExtension ) ){
	if( Output==1 | Output %in% c("o","object" ) ){ OutputExtension=".Pitch" }
	if( Output==2 | Output %in% c("t","tier"   ) ){ OutputExtension=".PitchTier" }
	if( Output==3 | Output %in% c("l","listing") ){ OutputExtension=".txt" }
}else{ # i.e. if the user provides their own OutputExtension
	if(substring(OutputExtension,first=1,last=1)!="."){ stop("The provided 'OutputExtension' must begin with a period.") }
	# Otherwise assume it is valid and accept it
} # End if/else OutputExtension is NULL

#...............
#.StartingRange.
#...............

# If the user over-rides 'StartingRange', make sure it is valid
if( length(StartingRange)!=2 ){stop("'StartingRange' must be of length 2.")} # Enforce it to be of length 2
if( identical( StartingRange, sort(StartingRange) ) ){ StartingRange <- sort(StartingRange) } # If it's in reverse order (i.e. [ceiling,floor]), sort it back to normal order: [floor,ceiling].

#...........
#.Dataframe.
#...........

# If the user provides something for 'Dataframe'...
if(!missing(Dataframe)){
	# Make sure the dataframe is in the right format.
	# More specifically, make sure it has three columns named appropriately and that the filenames match up
	if( ! identical( colnames(Dataframe), c("Filename","Floor","Ceiling"))){
		stop("The provided dataframe should have 3 (and only 3) columns:\n       'Filename', 'Floor', and 'Ceiling'.")
	}
	if( ! identical( as.character(Dataframe$Filename), FileList) ){
		stop("The file names in the provided Dataframe do not match\n       the set of soundfiles inside the specified 'WaveFolder'.")
	}
	# If the above two checks are passed, then extract out the old/previous Floor and Ceiling vectors
	Floors = Dataframe$Floor
	Ceilings = Dataframe$Ceiling
}else{ # i.e. if the user does NOT provide anything for 'Dataframe'
	# Create new, empty vectors to store the ultimately-determined Floor and Ceiling values
	Floors <- Ceilings <- rep(NA,times=nFiles)
} # End 'if/else something was provided for dataframe

############################
# Miscellaneous setting-up #
############################

# Set up the color scale to be used for the F0 points
# At present, the only option is to use an identical scale to the one used in the RichVisualization() function
ColorGenerator = colorRampPalette(rev(c("red", "orange", "yellow", "green", "blue", "#4B0082"))) # #4B0082" = Indigo.
# (Violet is used for secondary points.)
ColorSet = ColorGenerator(256) # A vector of 256 colors

# Open the window that will contain the plot
dev.new(width=Width,height=Height)

# Set up the margins for the window
par(mai=c(0.4,0.6,0.8,0.6))

# The 'Skip' variable determines whether the call to ToPitch() and the plotting code should be skipped.
# Start off the loop with this set to FALSE, hence the first file will be processed normally and properly.
Skip=FALSE

################################
# Begin loop through each file #
################################

for(EachFile in StartingIndex:nFiles){

# Provide an update as to their overall progress through the set of files.
# Format: File [CURRENT] of [TOTAL] ([PERCENT]%, [NUMBER] left): [FILENAME]
PercentDone = round(100*(EachFile/nFiles),1)
nLeft = nFiles - EachFile
ProgressText = paste( "File ", EachFile, " of ", nFiles, " (", PercentDone, "%, ", nLeft, " left): ", FileList[EachFile]," \n", sep="")
cat(ProgressText, sep="")
flush.console()

# Determine the name of the file currently being processed
CurrentWaveFile = FileList[EachFile]
FullWavePath = paste(WaveFolder,CurrentWaveFile,sep="")
# The input validation above guarantees that WaveFolder ends in a slash or backslash, so it is safe to do this.

# Extract out the 'core' of this filename, i.e. the portion before the file extension.
# Note that The input validation guarantees that every file ends in either '.wav' or 'WAV', so swapping out the final three characters will always work OK
nCharacters_CurrentWaveFile = nchar(CurrentWaveFile)
FilenameCore = substring( CurrentWaveFile, first=1, last=nCharacters_CurrentWaveFile-4 )

# Use this information to create file paths to the necessary pitch-related output files.
FullPitchPath_Initial = paste(PitchFolder,FilenameCore,".Pitch",sep="")
FullPitchPath_Final   = paste(PitchFolder,FilenameCore,OutputExtension,sep="")
# In the case of Output='object'/'o'/1, these will only be different if the user manually specifies an OutputExtension'

# Make a variable to indicate whether the output Pitch object needs to be 'converted' (to a PitchTier or Pitch Listing
if( Output==1 | Output %in% c("o","object" ) ){ 
	ConvertOutput=FALSE
}else{ # i.e. if( Output %in% 2:3 | Output %in% c("t","tier", "l","listing")
	ConvertOutput=TRUE
	# Note that it is technically possible for the user to request a pitch tier or pitch listing and yet specify the OutputExtension to be '.Pitch'. In this obscure case, a file will be written once and then another over-written again (both to the same path).
} # End if/else the output is such that it needs to be converted

# If there is a file already existing at the FullPitchPath_Initial and the user has not given explicit permission to over-write it, issue a warning
if( file.exists(FullPitchPath_Initial) & !Overwrite ){	
	stop(paste("A file already exists at the following path:\n      ",FullPitchPath_Initial,"\n       If you wish to over-write it, set 'Overwrite=TRUE'."))
} # End 'if this will overwrite something at FullPitchPath_Initial'

# If the output will be converted, do the same check on FullPitchPath_Final as well
# Thus, two checks in total will be performed. The same switch 'Overwrite' is used for both.
if(ConvertOutput){
	if( file.exists(FullPitchPath_Final) & !Overwrite ){	
		stop(paste("A file already exists at the following path:\n      ",FullPitchPath_Final,"\n       If you wish to over-write it, set 'Overwrite=TRUE'."))
	}
} # End 'if this will overwrite something at FullPitchPath_Final'

# Use 'StartingRange' for the current F0 range
CurrentRange = StartingRange

# Set SKIP to FALSE again
Skip=FALSE

# Create the "Feedback" variable and set it to an empty character string
z=""

#################################################
# Open repeat{} loop for analyzing current file #
#################################################

repeat{

if(Skip==FALSE){ # Only if Skip is FALSE...

# Execute PraatR command to create the initial Pitch object
ToPitch(Input=FullWavePath,Output=FullPitchPath_Initial,Overwrite=TRUE,Range=CurrentRange)
# Overwrite has to be TRUE in order for the trial-and-error iterative writing and re-writing of the Pitch object to work.

# Use the ReadPitch() function (also included in this package) to bring in the information contained in the Pitch object just created
Frames = ReadPitch(FullPitchPath_Initial,Return="Frames")
Candidates = ReadPitch(FullPitchPath_Initial,Return="Candidates")

# Map candidate strength onto color in the same way as it is done in the RichVisualization() function (also included in this package)
Strength_Intermediate = Frames$Strength - 0.5 # Shift distribution so it tops out at 0.5
Strength_Intermediate[Strength_Intermediate < 0 ] <- 0 # Snap the lower tail of the distribution to 0. (Mostly unvoiced frames.) This makes them all exactly the color blue.
NormalizedStrength = 2*Strength_Intermediate # Rescale from [0, 0.5] to [0, 1]
Colors = ColorSet[round(255*NormalizedStrength)+1] # 1 to 256

Title = paste(CurrentRange,collapse=" - ")
plot( x=Frames$Time, y=Frames$Frequency, las=1, cex=Frames$Intensity*1.5, pch=16, xlab="", ylab="", col=Colors, main=Title, yaxt="n", yaxs="i" )
# Intensity-CharacterExpansion mapping is the same as in RichVisualization()

# Define a sequence of numbers to be used for the axis and major gridlines
# Have this extend from 10 to (1000 or the ceiling for the starting range (rounded up to the nearest 10) - whichever is higher)
GridlineMax = max(10*ceiling(StartingRange[2]/10), 1000)
MajorGridlineSequence = seq(from=10,to=GridlineMax,by=10)

# Add a vertical axis (for the F0)
axis(side=2,at=MajorGridlineSequence,las=1)

# Add thicker horizontal grey lines at the major gridlines
abline(h=MajorGridlineSequence,lwd=2,col="#00000022")

# Add thinner horizontal grey lines at the minor gridlines
abline(h=MajorGridlineSequence-5,lwd=1,col="#00000022")

# Determine the empirical F0 range (based on the actually-attested points) and include that information in the plot
F0Range = range(Frames$Frequency,na.rm=TRUE)
abline(h=F0Range,col=c("blue","red"),lty="dashed")
text(x=rep(par("usr")[2],2),y=F0Range,labels=paste("",round(F0Range,1)),xpd=TRUE, adj=c(0,0.5), col=c("blue","red")) # Add a space before the numbers so it doesn't overlap with the box.

# Define secondary candidates and plot them
# (See RichVisualization() for details)
FirstVoiceless = Candidates[Candidates$nthCandidate==1 &  is.na(Candidates$Frequency), "nthFrame"]
SecondVoiced =   Candidates[Candidates$nthCandidate==2 & !is.na(Candidates$Frequency), "nthFrame"]
TargetFrames = intersect(FirstVoiceless,SecondVoiced)
TargetRowFilter = ( Candidates$nthFrame %in% TargetFrames ) & ( Candidates$nthCandidate==2 ) # For the kinds of frames just identified, I'm looking for the second candidate
Secondary.nthFrame = Candidates$nthFrame[ TargetRowFilter ]
SecondaryF0 = Candidates$Frequency[ TargetRowFilter ]
SecondaryStrength = Candidates$Strength[ TargetRowFilter ]
SecondaryIntensity = Frames$Intensity[Secondary.nthFrame]
SecondaryTime = Frames$Time[Secondary.nthFrame]
VioletScale = colorRampPalette(c("white","violet"))
SecondaryColors = VioletScale(256)[round(255*SecondaryStrength)+1]
points(SecondaryTime,SecondaryF0,pch=16,cex=SecondaryIntensity*1.5,col=SecondaryColors) # With richer visualization

}else{ # i.e. if Skip is TRUE

Skip = FALSE # ... thus successfully skipping call to ToPitch() and the plotting code but immediately switching it back

}# End 'if/else Skip is TRUE' check

# Query user input from the keyboard
Feedback = readline()

# If the user types the key to move to next file (Default: "z")
if(Feedback == Next){
	# Store the current range inside the relevant vectors
	Floors[EachFile] <- CurrentRange[1]
	Ceilings[EachFile] <- CurrentRange[2]
	break # break out of loop
}else{ # i.e. the Feedback was not [Next]

	# If the user types the key to play the file (Default: "p")
	if(Feedback==Play){
		praat( "Play", input=FullWavePath )
		Skip=TRUE # No need to re-extract the F0 and re-plot
	}else{ # i.f. if the feedback was not [Play]

		# If the user types the key to snap the range to the current empirically-determined range (Default: "s")
		if(Feedback == Snap){

			# Use the current empirically-determined range, rounded 'outward' (i.e. floor rounded down and ceiling rounded up)
			SnappedFloor = floor(F0Range[1])
			SnappedCeiling = ceiling(F0Range[2])
			CurrentRange[1] <- SnappedFloor
			CurrentRange[2] <- SnappedCeiling

			# Print to the console what values were thus determined
			cat("[Snapped to ",SnappedFloor,"-",SnappedCeiling,"]\n",sep="")
			flush.console()
			
			# Now try again (with Skip set to FALSE, hence F0 will be re-extracted and plotting will be re-done)
			next
			
		}else{ # i.e. if the feedback was not [Snap]
			if(Feedback == Quit){
				break # Break out of the return loop
			}else{
				# If the Feedback was neither [Next] nor [Play] nor [Snap] nor [Quit], it will be assumed the user is trying to hand-type an F0 range

				# The user needs to type their F0 range delimited by a dash, i.e. something like: "75-500"
				# Check that this is the case, and if not, issue a message to the user and elicit new input with another call to readline()
				Split = strsplit(Feedback,split="")[[1]]
				if( !("-" %in% Split | sum(Split=="-")>=2) ){ # If there aren't any dashes in the input, or if there is more than one
					cat("You need type an F0 range delimited by a dash, e.g. '75-500'.\n")
					flush.console()
					Skip=TRUE # ...and go through another loop of repeat{}, thus skipping the F0 extraction and plotting
				}else{
					DashIndex = which( Split == "-" )
					FirstHalf = paste(Split[1:(DashIndex-1)],collapse="")
					SecondHalf = paste(Split[(DashIndex+1):length(Split)],collapse="")
					FirstNumeric = as.numeric(FirstHalf) # Convert to numeric, but no validation to check that it makes sense as a numeric pitch range value
					SecondNumeric = as.numeric(SecondHalf)
					# Allow for the possibility of specifying them backwards
					if(FirstNumeric < SecondNumeric){
						SpecifiedFloor = FirstNumeric
						SpecifiedCeiling = SecondNumeric
					}else{
						SpecifiedFloor = SecondNumeric
						SpecifiedCeiling = FirstNumeric
					} # End 'if/else the first value is lower than the second'
					CurrentRange = c(SpecifiedFloor,SpecifiedCeiling)
				} # End 'if/else exactly 1 dash was found'
			} # End 'if/else the user hit [Quit]'
		} # End 'if/else the user hit [Snap]'
	} # End 'if/else the feedback was [Play]'
} # End 'if/else the feedback was [Next]'

} # End 'repeat{}' loop

#######################
# Tying up loose ends #
#######################

# Depending on what the user provides for 'Output', convert the file here to a PitchTier (in either long text or headerless spreadsheet format)
if(ConvertOutput){
	if( Output==2 | Output %in% c("t","tier"   ) ){ PraatOutputFormat="text" }
	if( Output==3 | Output %in% c("l","listing") ){ PraatOutputFormat="headerless spreadsheet" }
	praat("Down to PitchTier", input=FullPitchPath_Initial, output=FullPitchPath_Final, overwrite=Overwrite, filetype=PraatOutputFormat)
}else{ # i.f. if *not* converting the output
	if(OutputExtension!=".Pitch"){ # If the user requests a special OutputExtension for the Pitch object, rename appropriately.
		file.rename(from=FullPitchPath_Initial,to=FullPitchPath_Final)
	}
} # End 'if/elsez converting the output'

if(Delete){
	file.remove(FullPitchPath_Initial)
	# It goes to the Recycle Bin, so it's still recoverable in case of an accident
} # End 'if deleting'

if(Feedback==Quit){cat("The session has been terminated.\n");flush.console();break}
if(EachFile==nFiles){cat("All done!\n");flush.console()}
} # End 'each file' loop

# Return a dataframe containing all of the updated information
return( data.frame(Filename=FileList,Floor=Floors,Ceiling=Ceilings) )

#################################
# Arguments:                    #
#################################
# WaveFolder                    #
# PitchFolder                   #
# StartAt=1                     #
# Output="object"               #
# OutputExtension=NULL          #
# Overwrite=FALSE               #
# StartingRange=c(74.99,500.01) #
# Next="z"                      #
# Play="p"                      #
# Snap="s"                      #
# Quit="q"                      #
# Delete=FALSE                  #
# Dataframe                     #
# Width=10                      #
# Height=5                      #
#################################

} # End definition of function 'F0RangeFinder()'

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
