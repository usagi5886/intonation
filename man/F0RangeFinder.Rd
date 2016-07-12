\name{F0RangeFinder}
\alias{F0RangeFinder}

\title{
	Cycle through each soundfile in a folder and determine F0 range for each
}

\description{
	This function cycles through each soundfile in the specified \code{WaveFolder} and helps you quickly determine an optimal F0 range (i.e. 'floor' and 'ceiling' values) for each in a semi-automatic fashion.
	To do so, a window is created with a \code{\link{RichVisualization}}-style plot whose (unlabeled) axes are [y = F0 (in Hz), x = Time (in seconds)].
	The title of the plot displays the F0 range used to create the file.
	The F0 values for the highest and lowest points in the current F0 analysis are displayed in the top right (in red) and bottom right (in blue).
	Thin gridlines appear at every 5 Hz, and thick gridlines appear at every 10 Hz, to visually guide you.
	You can then type various commands into the R console:
	\enumerate{
		\item{Type an F0 range delimited by a dash, i.e. something like \code{"75-500"}, to re-run the F0 analysis with that range.}
		\item{\code{z} moves to the next file (and saves the F0 range currently displayed in the title of the plot).}
		\item{\code{p} plays the current soundfile.}
		\item{\code{s} 'snaps' the F0 range to the range defined by the points in the current plot.}
		\item{\code{q} quits the analysis at any time (e.g. to take a break).}
	}
	Your decisions are stored in three ways:
	\enumerate{
		\item{All relevant information is printed to the R console, hence every step of your analysis can be reconstructed by glancing over the history in the R console.}
		\item{Depending on the \code{Output} argument, a Pitch Object, PitchTier, or Pitch Listing is created in \code{PitchFolder} for every file you process, using the floor and ceiling values you ultimately decided on.}
		\item{A dataframe is returned containing the filenames for every file you processed as well as each's floor and ceiling values.  For details, see 'Value' section below.}
	}
}

\usage{F0RangeFinder(
	WaveFolder,
	PitchFolder,
	StartAt=1,
	Output="object",
	OutputExtension=NULL,
	Overwrite=FALSE,
	StartingRange=c(74.99,500.01),
	Next="z",
	Play="p",
	Snap="s",
	Quit="q",
	Delete=FALSE,
	Dataframe,
	Width=10,
	Height=5
)}

\arguments{
	\item{WaveFolder}{Path to the folder on your computer that contains the soundfiles to be processed. May not contain any spaces, and must end in either a slash ('\code{/}') or a backslash ('\code{\\}'). Each soundfile therein must end in either a \code{.wav} or \code{.WAV} file extension.}
	\item{PitchFolder}{Path to the folder on your computer where you would like to save the various pitch-related files that are created as a byproduct of using this function. May not contain any spaces, and must end in either a slash ('\code{/}') or a backslash ('\code{\\}').}
	\item{StartAt}{Which soundfile to start processing from. Can be specified either as an index (e.g. \code{5} for the 5th file) or as a filename (e.g. \code{MySound.wav}). Defaults to \code{1}, hence it will start at the first file and work through all of them.}
	\item{Output}{The storage format of the pitch-related files that are created as a byproduct of using this function. Can be set to values in three different formats: \code{"object"}/\code{"tier"}/\code{"listing"} \code{"o"}/\code{"t"}/\code{"l"}, or \code{1}/\code{2}/\code{3}. See 'Output' section below for more details.}
	\item{OutputExtension}{Determines the file extension of the output file. Must begin with a period. By default (i.e. if OutputExtension is left \code{NULL}, it depends on the \code{Output} argument. If \code{"object"} (or equivalent), it is .Pitch. If \code{"tier"}, it is .PitchTier. If \code{"listing"}, it is '.txt'.}
	\item{Overwrite}{If any files already exist in PitchFolder, defines whether over-writing should occur. Defaults to \code{FALSE}, thus protecting against accidental loss of data.}
	\item{StartingRange}{A numeric (or integer) vector of length 2 specifying the F0 range to be used as the first approximation for every file. Defaults to \code{c(74.99, 500.01)}, i.e. effectively [75, 500] except the values are bumped by +/-0.01 as an explicit marker that these are the initial values (and not an ultimately-decided F0 range).}
	\item{Next}{The text to type to move to the next file. Defaults to \code{"z"} (since this key is on the corner of the keyboard and close to the 's' key for \code{Snap}).}
	\item{Play}{The text to type to play the soundfile. Defaults to \code{"p"}.}
	\item{Snap}{The text to type to 'snap' the F0 range to the range defined by the points in the current plot (i.e. the minimum and maximum F0 values of the observed points). Defaults to \code{"s"}.}
	\item{Quit}{The text to quit the analysis at any time (e.g. to take a break). Defaults to \code{"q"}.}
	\item{Delete}{Whether the .Pitch objects created as a byproduct of using this function should be deleted. Useful to clean up unnecessary clutter in \code{PitchFolder}, especially if \code{Output} is set to \code{"tier"} or \code{"listing"}.}
	\item{Dataframe}{A dataframe created from a previous run of this function. Useful for picking up where one left off, e.g. after taking a break. For details, see 'Value' section below.}
	\item{Width}{Width of the window to be created. Defaults to 10.}
	\item{Height}{Height of the window to be created. Defaults to 5.}
}

\details{
	\enumerate{
		\item{WaveFolder and PitchFolder are the only arguments that must be specified. It is recommended that they be set to two 'sister' folders to each other (hence one should *not* be a subdirectory of the other).}
		\item{The filename root will be shared between the wave file its various derivative pitch files. Thus, if a wave file is called \code{MySound.wav}, its associated Pitch object, for instance, will be called \code{MySound.Pitch}.}
		\item{While using this function, the console should be kept visible on the screen so that it can be typed into. As such, once the window pops up, it is recommended to split the screen 50-50 between the F0RangeFinder() window and the R console.}
		\item{It is recommended to provide F0 ranges in the format [Floor,Ceiling], but it is fine if these are switched and instead the information is provided in the order [Ceiling,Floor]. (Both orders are supported throughout the code.)}
	}
}

\section{Output}{
The pitch-related files that are created as a byproduct of using this function can be stored in three different formats:
	\enumerate{
		\item{If \code{Output} is set to \code{"object"}, \code{"o"}, or \code{1}, it will be in Pitch Object format (which stores all the rich details of Praat's F0 analysis).}
		\item{If \code{Output} is set to \code{"tier"}, \code{"t"}, or \code{2}, it will be in PitchTier format, which only contains information about the highest-ranked candidate in voiced frames.}
		\item{If \code{Output} is set to \code{"listing"}, \code{"l"}, or \code{3}, it will be in (headerless) Pitch Listing format - which contains the same information as a PitchTier but is tab-delimited (and hence is easy to read into R and copy-paste into spreadsheet software).}
	}
	The default is \code{"object"} since the conversion to the other two formats involves a loss of information.
}

\value{
Returns a dataframe with three columns: \code{Filename}, \code{Floor}, and \code{Ceiling}.
When running this function, the result should be stored in a variable, e.g. \code{x=F0RangeFinder(...)}.
This can then be passed to a future run of the \code{F0RangeFinder()} function via the \code{Dataframe} argument.
Due to this functionality, it is possible to stop in the middle of one's analysis (e.g. for a break) and later pick up from where your left off.
When doing so, simply specify the \code{StartAt} argument to indicate where you would like to resume from.
}

\note{
Note that while the \code{Next} argument determines how to proceed to the next file, it is not possible to go backwards to the previous ile.
If it ever becomes necessary to do so, simply do the [Quit] command (e.g. \code{"q"}) to terminate the code and then adjust the "StartAt" index accordingly.}

\references{
	This function is not directly referenced in, but was used for, the following dissertation:
	\itemize{
		\item{Albin, A. (2015). \emph{Typologizing native language influence on intonation in a second language: Three transfer phenomena in Japanese EFL learners}. (Doctoral dissertation). Indiana University, Bloomington. \href{http://hdl.handle.net/2022/20345}{http://dx.doi.org/10.5967/K8JW8BSC}}
	}
}

\author{
	Aaron Albin (\url{http://www.aaronalbin.com/})
}

\seealso{
	This function draws on \code{\link{ToPitch}} and \code{\link{ReadPitch}} for the F0 analyses.
	The plotting recycles much code from \code{\link{RichVisualization}}.
	Much of this function draws on the \href{http://www.aaronalbin.com/praatr/}{PraatR} package (which is a dependency for the 'intonation' package as a whole).
}

\examples{
\dontrun{

# Basic usage
F0Ranges1 = F0RangeFinder(
	WaveFolder="C:/Users/MyUsername/Desktop/Wave/",
	PitchFolder="C:/Users/MyUsername/Desktop/Pitch/"
)

# Resuming from where you left off (e.g. on file 12)
F0Ranges2 = F0RangeFinder(
	WaveFolder="C:/Users/MyUsername/Desktop/Wave/",
	PitchFolder="C:/Users/MyUsername/Desktop/Pitch/"
	StartAt=12,
	Dataframe=F0Ranges1
)

# Saving in a PitchTier format and making it have a '.txt' extension
F0Ranges3 = F0RangeFinder(
	WaveFolder="C:/Users/MyUsername/Desktop/Wave/",
	PitchFolder="C:/Users/MyUsername/Desktop/Pitch/"
	Output="tier",
	OutputExtension=".txt"
)
}}

\keyword{ interface }
\keyword{ file }
