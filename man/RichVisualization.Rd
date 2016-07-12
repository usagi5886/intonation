\name{RichVisualization}
\alias{RichVisualization}

\title{
	Generate a 'rich visualization' plot
}

\description{
	This function takes as its input a path to a soundfile and corresponding Pitch object on one's computer and produces a 'rich visualization' of the F0 data, as described in Chapter 3 of \href{http://hdl.handle.net/2022/20345}{Albin (2015)}.
}

\usage{
RichVisualization( PitchPath,
                   WavePath,
                   SameWindow = FALSE,
                   Width = 6.5,
                   Height = 4.33,
                   xlim,
                   F0Range,
                   Labels,
                   Divisions_ms,
                   TextGridPath,
                   TextGridTier,
                   SilenceLabel,
                   Grayscale = FALSE,
                   Spectrogram_FontSize = 1.5,
                   Spectrogram_ylim,
                   Spectrogram_col = c("white", "grey75"),
                   ...
)
}

\arguments{
	\item{PitchPath}{Full path to a Pitch object textfile on one's computer, which must be stored in Praat's (long) "text" format. This is used to create the F0 track.}
	\item{WavePath}{Full path to a WAV file on one's computer. This is used to create the waveform and spectrogram.}
	\item{SameWindow}{Logical variable indicating whether to create a new window (\code{SameWindow=FALSE}) or inherit an existing one (\code{SameWindow=TRUE}).}
	\item{Width}{Specifies the width of the new window to be created. Defaults to 6.5 inches, which fills up the horizontal space on a 8.5x11 inch paper with 1-inch margins.}
	\item{Height}{Specifies the height of the new window to be created. Defaults to 4.33 inches, i.e. (roughly) two-thirds the width (i.e. 6.5*2/3).}
	\item{xlim}{x axis limits, i.e. the time range to be plotted. This is shared across all three panels in the plot (waveform, F0 track, and spectrogram). If left unspecified, the full time range of the soundfile is used.}
	\item{F0Range}{The F0 range (in Hertz) for the F0 track panel of the plot. If left unspecified, the floor is the lowest (first-candidate) F0 point stored in the Pitch object, and the ceiling is taken from the header of the Pitch object.}
	\item{Labels}{A character vector (of any length) containing the label(s) to go in the boxes of the spectrogram pane, representing some linguistic unit (segments, syllables, words, etc.)}
	\item{Divisions_ms}{A numeric vector with the timestamps (in milliseconds) of the vertical bars to be drawn between the segments/syllables/words/etc.}
	\item{TextGridPath}{Path to a TextGrid on one's computer from which the segmentation should be drawn.}
	\item{TextGridTier}{The name of an interval tier containing the segmentation boundaries and labels, e.g. \code{"words"}. (Does nothing if 'TextGridPath' is left unspecified.)}
	\item{SilenceLabel}{Some 'silence label' (e.g. \code{"<SIL>"}) that should be erased (i.e. changed to "") when plotting the segmentation.}
	\item{Grayscale}{Logical variable indicating whether the F0 track should be plotted in grayscale. Defaults to \code{FALSE} (full color). (Note the spelling is 'Grayscale' - with an 'a', not an 'e'.)}
	\item{Spectrogram_FontSize}{Font size (i.e. 'character expension', or 'cex') for the text labels in the spectrogram. Defaults to 150\% (to make the labels easy to read).}
	\item{Spectrogram_ylim}{y axis limits for the spectrogram. A vector of length 2, with the minimum and maximum frequency of the range to be plotted. If left unspecified, defaults to [min=0, max=the soundfile's Nyquist frequency].}
	\item{Spectrogram_col}{Color palette for the spectrogram. Defaults to a faded grayscale (to ensure the labels are can be easily read). For how to specify other color palettes, see "Colors" section in the help file for \code{\link{Spectrogram}}.}
	\item{\dots}{Any other arguments are passed to Spectrogram(). Arguments that are commonly tweaked include \code{WindowLength}, \code{FrequencyResolution}, \code{nTimeSteps}, and \code{DynamicRange}.}
}
\details{
	\itemize{
		\item{If \code{Labels} and/or \code{Divisions_ms} is left unspecified, no labels or divisions will be drawn (hence the spectrogram pane will contain just the spectrogram alone, without any superimposed lines or text).}
		\item{\code{Divisions_ms} should be \code{length(Labels)+1} components long and in strictly increasing/ascending order.}
		\item{The first and/or last values inside \code{Divisions_ms} can be made to match up to the range of time specified for \code{xlim} by specifying them as \code{NA}. For example, if \code{xlim=c(1,5)}, then \code{Divisions_ms=c(NA,2,3,4,NA)} is equivalent to \code{Divisions_ms=c(1,2,3,4,5)}.}
		\item{If \code{TextGridPath} is provided, it overrides any specification of the \code{Labels} and \code{Divisions_ms} arguments (with a warning).}
	}
}

\section{Silence label}{
	Corpus annotations often have a symbol specifically for explicitly marking silence, e.g. \code{"<SIL>"}. However, when viewing a plot, the fact a given interval represents silence is clear enough if that interval is simply left empty. Indeed, leaving the silence labels in can clutter up the segmentation, especially if the silent intervals are short. Thus, the argument \code{SilenceLabel} can be set to any character string to have all instances of that label changed to \code{""} (an empty string) in the segmentation. Since there is no default value for this argument, the silence label is transparently printed as-is unless the user requests otherwise.
}

\value{
	None. This function is used for its side effect - displaying a 'rich visualization' plot.
}

\references{
	This function is referenced on pages 58-65 of
	\itemize{
		\item{Albin, A. (2015). \emph{Typologizing native language influence on intonation in a second language: Three transfer phenomena in Japanese EFL learners}. (Doctoral dissertation). Indiana University, Bloomington. \href{http://hdl.handle.net/2022/20345}{http://dx.doi.org/10.5967/K8JW8BSC}}
	}
}

\author{
	Aaron Albin (\url{http://www.aaronalbin.com/})
}

\note{
	\itemize{
		\item{Note that, if \code{SameWindow=TRUE}, it is assumed (but not verified) that the inherited previous window was also a RichVisualization (with its unique settings for margins, number of panels, etc.)}
		\item{The \code{Width} and \code{Height} arguments only do anything if \code{SameWindow=FALSE}. If \code{SameWindow=TRUE}, the values for \code{Width} and \code{Height} are ignored (with a warning)}
	}
}

\section{Warning}{
	Even if \code{xlim} is specified, R will still process the full time range of the soundfile, hence the processing time will be slow on long soundfiles.
}

\seealso{
	This function uses \code{\link{ReadPitch}} to generate the F0 track (center panel) and \code{\link{Spectrogram}} to generate the segmentation (bottom panel). Once a rich visualization plot has been created, a stylization can be created with \code{\link{Stylize}} and then plotted with \code{\link{PlotStylization}}. Note also that the same kind of rich visualziation representation is used in \code{\link{F0RangeFinder}}.
}

\examples{
# Create paths to the sample data (WAV file, Pitch object textfile, and TextGrid) included in the package
WavePath = paste(R.home("library"),"intonation","HelloWorld.wav",sep="/")
PitchPath = sub(WavePath,pattern=".wav",replacement=".Pitch",fixed=TRUE)
TextGridPath = sub(WavePath,pattern=".wav",replacement=".TextGrid",fixed=TRUE)

# At a bare minimum, you must provide a PitchPath and a WavePath
RichVisualization(PitchPath=PitchPath, WavePath=WavePath)

# Running the same command again opens a new window
RichVisualization(PitchPath=PitchPath, WavePath=WavePath)

# Set SameWindow=TRUE to have successive rich visualization plots share the same window
RichVisualization(PitchPath=PitchPath, WavePath=WavePath, SameWindow=TRUE)

# Adjust the size of the window (to 150\% of the normal size)
RichVisualization(PitchPath=PitchPath, WavePath=WavePath, Width = 9.75, Height = 6.5)

graphics.off() # (Close all open windows before moving on to the next example)

# Set the x axis limits to 100-665 ms (to zoom into the word 'hello'
RichVisualization( PitchPath=PitchPath, WavePath=WavePath, xlim=c(100,665) )

# (All instances below have 'SameWindow=TRUE' to avoid proliferation of windows.)

# Adjust the F0 range to zoom into a specific frequency range
RichVisualization( PitchPath=PitchPath, WavePath=WavePath, SameWindow=TRUE, xlim=c(100,665), F0Range=c(69,100) )

# Provide the segmentation manually
RichVisualization( PitchPath=PitchPath, WavePath=WavePath, SameWindow=TRUE,
                   Labels = c("hello","world"), Divisions_ms = c(132,648,1257) )

# Provide the segmentation via TextGridPath and TextGridTier
RichVisualization( PitchPath=PitchPath, WavePath=WavePath, SameWindow=TRUE,
                   TextGridPath=TextGridPath, TextGridTier="words" )

# Note that "<SIL>" is showing at the beginning and end of the file (since these are contained in the TextGrid)
# Hide these 'silence labels' using the SilenceLabel argument
RichVisualization( PitchPath=PitchPath, WavePath=WavePath, SameWindow=TRUE,
                   TextGridPath=TextGridPath, TextGridTier="words",
                   SilenceLabel="<SIL>" )

# Change the plot to grayscale (e.g. for publishing in a journal with a no-color policy)
RichVisualization( PitchPath=PitchPath, WavePath=WavePath, SameWindow=TRUE,
                   Grayscale=TRUE )

# Increase the spectrogram's font size (e.g. to accompany changes to the overall window size via the 'Height' and 'Width' arguments)
RichVisualization( PitchPath=PitchPath, WavePath=WavePath, SameWindow=TRUE,
                   Labels = c("hello","world"), Divisions_ms = c(132,648,1257), 
                   Spectrogram_FontSize = 2.5 )

# Adjust the y axis limits for the Spectrogram (so it is zoomed in to 0-2000 Hz)
RichVisualization( PitchPath=PitchPath, WavePath=WavePath, SameWindow=TRUE, Spectrogram_ylim=c(0,2000))
# Adjusting this can be useful if the recording is of high quality and stretches into high frequencies beyond the range used for speech

# Change the color palette for the spectrogram
RichVisualization( PitchPath=PitchPath, WavePath=WavePath, SameWindow=TRUE,
                   Spectrogram_col=NULL )
# (Spectrogram_col=NULL triggers the default color scheme for the Spectrogram() function.)

# Adjust other advanced parameters of the spectrogram
RichVisualization( PitchPath=PitchPath, WavePath=WavePath, SameWindow=TRUE,
                   WindowLength=50, FrequencyResolution=8, TimeStepSize=1, DynamicRange=50 )
}

\keyword{ hplot }
\keyword{ file }
