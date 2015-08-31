##################################
# ReadPitch()                    #
# ------------------------------ #
# Copyright (C) 2015 Aaron Albin #
# http://www.aaronalbin.com/     #
##################################

# Takes a Pitch object textfile, reads it into R in a quick and efficient way, and returns the requested information.

# This function is referenced on page 55 of:
# Albin, A. (2015). Typologizing native language influence on intonation in
#     a second language: Three transfer phenomena in Japanese EFL learners.
#     (Doctoral dissertation). Indiana University, Bloomington.
#     http://dx.doi.org/10.5967/K8JW8BSC

ReadPitch = function( # Begin argument list

# [1] File path
File, # Path to a Pitch object stored on one's computer
# This must be stored in Praat's "text" format (and not "short (text)" or "binary" format).
# This does *not* have to be a full path, and it *can* have spaces in it.

# [2] 'Return'
Return="Frames" # Specifies the kind of information about the Pitch object to be brought into R.
# At present, three values are accepted:
# - 'Header': All the info from the header, in the same order (but with more transparent names)
# - 'Frames': Information about each frame (and each frame's highest-ranking candidate)
# - 'Candidates': Information about every candidate in each frame
# The default is 'Frames' since it represents the most common use case.

){ # Begin body of function

# First, do some basic input validation
# Check whether a file indeed exists at the specified file path
if( !file.exists(File) ){stop("The specified file does not exist.")}
# Note that it is assumed (but not explicitly verified) that this file is indeed a Pitch object.

# Make sure the value supplied for 'Return' is a permissible choice
if( !( Return %in% c("Header","Frames","Candidates") ) ){
	stop("The argument 'Return' must be set to 'Header', 'Frames', or 'Candidates'.")
} # End 'if' statement

# Now proceed with the function proper...

# Read in the file
if(Return=="Header"){ # If 'Return' is 'Header'...
	nLines=10 # ...read in just the first 10 lines (to save time).
}else{ # Otherwise, read in the whole thing.
	nLines=-1 # -1 is the default for readLines(), telling R to read in the whole file
} # End 'if/else'
FullText = readLines(File,n=nLines)

# Now extract all of the information from the header (i.e. the first 10 lines of the Pitch object).
# The information in the header looks like the following:

# [01] File type = "ooTextFile"
# [02] Object class = "Pitch 1"
# [03] 
# [04] xmin = 0 
# [05] xmax = 3.3430625 
# [06] nx = 331 
# [07] dx = 0.01 
# [08] x1 = 0.02153125000000011 
# [09] ceiling = 600 
# [10] maxnCandidates = 15 

FileStartTime       = as.numeric(strsplit(FullText[ 4],split=" ")[[1]][3])
FileEndTime         = as.numeric(strsplit(FullText[ 5],split=" ")[[1]][3])
nFrames             = as.integer(strsplit(FullText[ 6],split=" ")[[1]][3])
TimeStep            = as.numeric(strsplit(FullText[ 7],split=" ")[[1]][3])
FirstTime           = as.numeric(strsplit(FullText[ 8],split=" ")[[1]][3])
Ceiling             = as.integer(strsplit(FullText[ 9],split=" ")[[1]][3])
MaxNumberCandidates = as.integer(strsplit(FullText[10],split=" ")[[1]][3])

if(Return=="Header"){ # Begin outer if/else...

# Return the information from the header in the form of a list
# The list format ensures everything prints 'pretty' (e.g. with integers treated as integers).
# Moreover, the entries in the list can still be easily accessed with '$'.
return( list( FileStartTime=FileStartTime,
                            FileEndTime=FileEndTime,
                            nFrames=nFrames,
                            TimeStep=TimeStep,
                            FirstTime=FirstTime,
                            Ceiling=Ceiling,
                            MaxNumberCandidates=MaxNumberCandidates ) )
# Note that the components of this list are in the same order as in the actual header itself (just with different names)

# Note that, unfortunately, the floor is *not* stored inside the header of a Pitch object.
# It can be merely guessed at from lowest observed F0 candidate as follows:
#ApproximateFloor = round( min(F0Values,na.rm=TRUE) ) # Not exact!
# (Here, 'round()' is used instead of 'floor' because sometimes sampled F0 values can go under an actual specified floor a tiny bit.)

}else{ # i.e. if 'Return' is *not* 'Header'... (Outer if/else)

# Use the information from the header to reconstruct the time series
Time=FirstTime+TimeStep*0:(nFrames-1)

# Extract out the frequency of each candidate
FrequencyLines = grep(FullText,pattern="                frequency = ",fixed=TRUE,value=TRUE)
Frequency = as.numeric( substring(FrequencyLines,first=29,last=nchar(FrequencyLines)-1) )
Frequency[Frequency == 0] <- NA # Replace dummy '0' with (the more semantically transparent) NA

# Extract out the strength of each candidate
StrengthLines = grep(FullText,pattern="                strength = ",fixed=TRUE,value=TRUE)
Strength = as.numeric( substring(StrengthLines,first=28,last=nchar(StrengthLines)-1) )
Strength[Strength == 0] <- NA # Again, replace dummy '0' with NA.

# Extract out the intensity of each frame
IntensityLines = grep(FullText,pattern="        intensity = ",fixed=TRUE,value=TRUE)
Intensity = as.numeric( substring(IntensityLines,first=21,last=nchar(IntensityLines)-1) )

# Extract out the number of candidates in each frame
nCandidatesLines = grep(FullText,pattern="        nCandidates = ",fixed=TRUE,value=TRUE)
nCandidates = as.integer( substring(nCandidatesLines,first=23,last=nchar(nCandidatesLines)-1) )

# Use the 'nCandidates' information to construct a 'nthCandidate' vector
nthCandidate = unlist( lapply( 1:nFrames, FUN=function(x){1:nCandidates[x]}))

# Use all of the information assembled above to filter down the candidate-level Frequency and Strength vectors into two smaller vectors containing just the information for the first (i.e. highest-ranked) candidates
Frequency1 = Frequency[nthCandidate==1]
Strength1 = Strength[nthCandidate==1]
# Note: The '1' suffix in the variable names indicates 'ranked 1st'.

if(Return=="Frames"){ # Begin inner if/else...
# Return the information about each frame in the form of a dataframe
return( data.frame( # nthFrame=1:nFrames, # Excluded since largely redundant (and easy to reconstruct)
					Time=Time,
                    Frequency=Frequency1, # Note name change
                    Intensity=Intensity,
                    Strength=Strength1, # Note name change
                    nCandidates=nCandidates ) )

}else{ # i.e. if 'Return' is *not* 'Frames'... (Inner if/else)
# By this point, by virtue of the the input validation at the beginning of the function, it is safely guaranteed that 'Return' is 'Candidates'.

# Create an 'nthFrame' vector, indexing which frame the measurements are associated with
nthFrame = unlist( lapply( 1:nFrames, FUN=function(x){rep(x, times=nCandidates[x])}) )

# Return the information about the candidates in each frame in the form of a dataframe
return( data.frame( nthFrame=nthFrame,
                    nthCandidate=nthCandidate,
                    Frequency=Frequency,
                    Strength=Strength ) )

} # End inner if/else ('Return' is 'Frames'?)

} # End outer if/else ('Return' is 'Header'?)

###################
# Arguments:      #
###################
# File            #
# Return="Frames" #
###################

} # End definition of function 'ReadPitch()'

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
