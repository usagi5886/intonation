\name{ToPitch}
\alias{ToPitch}

\title{
	Create a Pitch object
}

\description{
	This function takes a soundfile, conducts an F0 analysis, and produces a Praat-format Pitch object using various customizable analysis parameters. As such, this function serves as a high-level interface to Praat's "Sound: To Pitch..." command, using the \href{http://www.aaronalbin.com/praatr/}{PraatR} package
}

\usage{
ToPitch( Input,
         Output,
         Overwrite = FALSE,
         FileType = "text",
         Range,
         TimeStep = 0.001,
         MaxNumberOfCandidates = 15,
         VeryAccurate = FALSE,
         SilenceThreshold = 0.03,
         VoicingThreshold = 0.45,
         OctaveCost = 0.01,
         OctaveJumpCost = 0.35,
         VoicedUnvoicedCost = 0.14
)
}

\arguments{
	\item{Input}{Path to a WAV file on one's computer containing the audio data from which the F0 should be extracted.}
	\item{Output}{Path to the Pitch object textfile that will be created as an end-product of using this function.}
	\item{Overwrite}{Logical variable determining whether the file will be overwritten if the \code{Output} file already exists on your computer. Defaults to \code{FALSE} to protect against accidental loss of data.}
	\item{FileType}{Which of Praat's output formats the Pitch object data should be saved as - either \code{"text"}, \code{"short text"} (=\code{"short"}), or \code{"binary"}. Of these, \code{"binary"} saves the most disk space, whereas \code{"text"} (the default) makes the data structure most explicit (which is convenient for subsequent processing in R).}
	\item{Range}{The F0 range (in Hertz) for the F0 analysis, specified as a vector, e.g. \code{c(100, 200)} to set the range as 100-200 Hz. Note how this argument has no default, hence this must be explicitly specified.}
	\item{TimeStep}{The size of the time step (or 'frame') for the analysis, in milliseconds. The default of 0.001 means an F0 value will be extracted at every millisecond.}
	\item{MaxNumberOfCandidates}{The maximum number of candidate F0 values in each frame. The default value here (15) follows Praat's default. Setting this to 2 will save disk space and will not affect most analyses.}
	\item{VeryAccurate}{This and all of the remaining arguments are Praat's "advanced pitch settings", with default values matching those of Praat.}
	\item{SilenceThreshold}{"}
	\item{VoicingThreshold}{"}
	\item{OctaveCost}{"}
	\item{OctaveJumpCost}{"}
	\item{VoicedUnvoicedCost}{"}
}

\details{
	The \code{Input} and \code{Output} arguments must both be full file paths (i.e., you can't rely on R's working directory functionality). These file paths also must contain no spaces (due to a limitation in the current implementation of PraatR).
}
\value{
	None. This function is used for its side-effect: creating a new textfile at the location specified in \code{Output}.
}

\references{
	This function is referenced on page 55 of
	\itemize{
		\item{Albin, A. (2015). \emph{Typologizing native language influence on intonation in a second language: Three transfer phenomena in Japanese EFL learners}. (Doctoral dissertation). Indiana University, Bloomington. \href{http://hdl.handle.net/2022/20345}{http://dx.doi.org/10.5967/K8JW8BSC}}
	}
}

\author{
	Aaron Albin (\url{http://www.aaronalbin.com/})
}

\note{
	Praat has the following five different commands for extracting F0. (Of these, #1 is just a high-level convenience wrapper/interface to #2.)
	\enumerate{
		\item{To Pitch...}
		\item{To Pitch (ac)...}
		\item{To Pitch (cc)...}
		\item{To Pitch (SPINET)...}
		\item{To Pitch (shs)...}
	}
	The present function invokes Praat with command #2.
}

\seealso{
	To read the Pitch object textfile created by this function back into R, see \code{\link{ReadPitch}}.
}

\examples{
# Create path to sample wave file ('HelloWorld.wav') included in the package
WavePath = paste(R.home("library"),"intonation","HelloWorld.wav",sep="/")

# Swap out '.wav' for '.Pitch' to make a path where the Pitch object should be created
PitchPath = sub(WavePath, pattern=".wav", replacement=".Pitch", fixed=TRUE)

# Determine the F0 range for the file in question
Range = c(69,135)

\dontrun{

# Run the 'To Pitch...' command with default arguments
ToPitch( Input=WavePath, Output=PitchPath, Range=Range )

# To allow over-writing of files, set 'Overwrite' to TRUE
ToPitch( Input=WavePath, Output=PitchPath, Range=Range, Overwrite=TRUE )

# To save in another Praat export format, set the 'FileType' argument
ToPitch( Input=WavePath, Output=PitchPath, Range=Range, FileType="short text" ) # Equivalent to "short"
ToPitch( Input=WavePath, Output=PitchPath, Range=Range, FileType="binary" )

# Adjust time step
ToPitch( Input=WavePath, Output=PitchPath, Range=Range, TimeStep=0.01 ) # 10 ms (sparser, hence faster)

# Adjust various advanced analysis parameters
ToPitch( Input=WavePath, Output=PitchPath, Range=Range, VeryAccurate=TRUE )
# MaxNumberOfCandidates, SilenceThreshold, VoicingThreshold, OctaveCost, OctaveJumpCost, and VoicedUnvoicedCost also be adjusted in a similar way
}

}

\keyword{ interface }
\keyword{ file }
