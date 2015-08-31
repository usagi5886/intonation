\name{Spectrogram}
\alias{Spectrogram}

\title{Generate a spectrogram}

\description{
	Takes audio data as input and generates a \href{https://en.wikipedia.org/wiki/Spectrogram}{spectrogram} from it, either by displaying a spectrograph plot or by returning a matrix of the underlying numbers thereof (depending on the value for the \code{plot} argument). The audio data can be provided in three different ways:
	\itemize{
	   \item{a path to a soundfile saved somewhere on your computer}
	   \item{an R object containing the information contained in this soundfile}
	   \item{a numeric vector containing the information inside the soundfile}
	}
	For details on how to specify the audio input to the function see the section "Input" below.
}

\usage{
Spectrogram( Audio,
             SamplingFrequency = NULL,
             WindowLength = 5,
             FrequencyResolution = 4,
             TimeStepSize = NULL,
             nTimeSteps = NULL,
             Preemphasis = TRUE,
             DynamicRange = 70,
             Omit0Frequency = FALSE,
             WindowType = "kaiser",
             WindowParameter = NULL,
             plot = TRUE,
             PlotFast = TRUE,
             add = FALSE,
             col = NULL,
             xlim = NULL,
             ylim = NULL,
             main = "",
             xlab = "Time (ms)",
             ylab = "Frequency (Hz)"
)
}

\arguments{
	\item{Audio}{The audio data from which the spectrogram is to be made. This can be specified in three different ways. (For details, see the "Input" section below.)}
	\item{SamplingFrequency}{The sampling frequency/rate of the sound (in Hertz). It is only necessary to specify this if the \code{Audio} input is provided using option #3 (a numeric vector).}
	\item{WindowLength}{The desired length (in milliseconds) of the analysis window used to create the spectrogram. Defaults to 5 milliseconds (thus generating a narrow-band spectrogram).}
	\item{FrequencyResolution}{A positive integer, such that higher numbers mean better resolution. (Specifically, for any integer \code{X} provided, \code{1/X} the analysis bandwidth, as determined by the number of samples in the analysis window, will be used.)}
	\item{TimeStepSize}{Number of milliseconds that the window will be moved for each adjacent frame in the analysis.}
	\item{nTimeSteps}{The overall total number of time steps (or frames) that the spectrogram analysis should be broken into.}
	\item{Preemphasis}{Logical variable indicating whether pre-emphasis should be applied (i.e. whether the spectral slope should increase by 6 dB per octave using a single-pole filter). This affects all regions of the frequency domain, i.e. the 'frequency cutoff' is trivially 0.}
	\item{DynamicRange}{Values in the spectrogram less than this many dB below the maximum intensity are 'clipped' to that value. If set to NULL, no such clipping occurs. This essentially determines the range of information to be captured in the color gradient for the z axis of the plot.}
	\item{Omit0Frequency}{The frequency band at 0 Hz is usually at very low values (e.g. -400 to -300 dB). Select \code{Omit0Frequency=TRUE} to omit this frequency band from the spectrogram. (This only makes a difference if \code{DynamicRange=NULL}.)}
	\item{WindowType}{A character string indicating the desired type of window function to be applied to the signal. See section "Window Types" below for the full list of supported window types.}
	\item{WindowParameter}{If \code{WindowType=gaussian} or \code{WindowType=kaiser}, this specifies the relevant parameter behind the function in question. If \code{WindowParameter=NULL}, defaults to \code{0.4} (for \code{WindowType=gaussian}) or \code{3} (for \code{WindowType=kaiser}).}
	\item{plot}{Logical variable indicating whether the spectrogram should be plotted or not. If \code{FALSE}, no spectrogram is plotted, and instead, a matrix is returned containing the magnitude at each bin center.}
	\item{PlotFast}{If \code{FALSE}, the \code{filled.contour()} function is used, which produces better looking graphics but takes considerably longer to plot. If \code{TRUE} (the default), the \code{image()} function is used instead, which makes the plotting much faster.}
	\item{add}{Logical variable indicating whether an entirely new plot is drawn (complete with axis labels/numbering/etc.) (\code{add=FALSE}) or whether just the core image is drawn (\code{add=TRUE}).}
	\item{col}{The color map for representing the z axis in the spectrogram plot. See section "Colors" below for the different ways this argument can be used.}
	\item{xlim}{x axis limits. If left at \code{NULL}, defaults to the full time range of the soundfile.}
	\item{ylim}{y axis limits. If left at \code{NULL}, extends from 0 to the soundfile's Nyquist frequency.}
	\item{main}{Main title for the plot. Defaults to \code{""} (i.e. nothing).}
	\item{xlab}{x axis label, by default \code{"Time (ms)"}}
	\item{ylab}{y axis label, by default \code{"Frequency (Hz)"}}
}

\section{Input}{
	Audio data cam be brought into R in the following ways:
	\itemize{
		\item{ With the \code{load.wave()} function in the \code{audio} package, thus creating an R object of class \code{audioSample}.}
		\item{ With the \code{loadsound()} function in the \code{phonTools} package, thus creating an R object of class \code{sound}.}
		\item{ With the \code{readWave()} function in the \code{tuneR} package, thus creating an R object of class \code{Wave}.}
		\item{ With the \code{loadSample()} function in the \code{sound} package, thus creating an R object of class \code{Sample}.}
	}
	If you have one of the above four packages (\code{audio}, \code{phonTools}, \code{tuneR}, or \code{sound}) installed, you can set \code{Audio} to a character string indicating the path to the relevant soundfile (saved somewhere on your computer). Alternatively, you can set \code{Audio} to an R object (of class \code{audioSample}, \code{sound}, \code{Wave}, or \code{Sample}) created with one of the above four functions. In addition, you can also set \code{Audio} to a numeric vector representing the sequence of samples in a (monoaural/single-channel) soundfile, in which case \code{SamplingFrequency} must also be specified. (Note that setting \code{Audio} to a 2-row or 2-column matrix, representing the two channels in a stereo soundfile, is currently unsupported.) Thus, there are a total of three different ways to use the \code{Audio} argument.
}

\value{
	\itemize{
		\item{ If \code{plot=FALSE}, returns a matrix containing the result of the analysis, with attributes indicating all the non-NULL argument specifications that influenced the result of the analysis}
		\item{ if \code{plot=TRUE} (the default), nothing is returned, and instead a plot of the spectrogram is produced The column names of this matrix correspond to time, and the row names correspond to frequency. Note that both are fully unrounded numbers stored as a character string (e.g. \code{"115.532879818594"}).}
	}
}

\section{Window Types}{
	All of the following types are supported:
	\itemize{
		\item{\code{"rectangular"} / \code{"square"}}
		\item{\code{"blackman"}}
		\item{\code{"hann"} / \code{"hanning"} (i.e. sine-squared)}
		\item{\code{"hamming"} (i.e. raised sine-squared)}
		\item{\code{"cosine"} / \code{"sine"} }
		\item{\code{"bartlett"}}
		\item{\code{"gaussian"}}
		\item{\code{"kaiser"}}
	}
	(Note that all names are in lowercase.)
}

\section{Colors}{
	The \code{col} argument can be used in four ways:
	\itemize{
		\item{If \code{col=NULL}, defaults to [dark blue, blue, cyan, yellow, orange, red, brown].}
		\item{If \code{col="alternate"}, the palette is [black, red, orange, yellow, white].}
		\item{If \code{col} is set to \code{"greyscale"} or \code{"grayscale"}, the palette is [white, black].}
		\item{You can also specify your own custom palette by providing a character vector with colors names, e.g. \code{col=c("green","blue","purple")}.}
	}
}

\details{
	\itemize{
		\item{You can specify either \code{TimeStepSize} or \code{nTimeSteps}, but not both. If you leave them both unspecified (as \code{NULL}), \code{nTimeSteps=400} will be used.}
		\item{\code{Preemphasis}, \code{DynamicRange}, and \code{Omit0Frequency} all affects the results regardless of whether the spectrogram is plotted (\code{plot=TRUE}) or a matrix is returned (\code{plot=FALSE}).}
		\item{If \code{plot=FALSE}, all of the arguments after \code{plot} in the arguments list will do nothing.}
		\item{If \code{PlotFast=TRUE}, \code{Spectrogram()} invokes the lower-level function \code{image()} with the argument \code{useRaster=TRUE}. This may not work properly if raster graphics are not supported on your device. See help("image") for details.}
		\item{\code{add} should only be set to \code{TRUE} if a spectrogram has already been drawn in an open graphics device. In this case, the coordinate system of the pre-existing plot is used (any any specification of \code{xlim} and/or \code{ylim} is ignored).}
	}
}

\note{
	The arguments \code{FrequencyResolution} and \code{TimeStepSize}/\code{nTimeSteps} greatly impact processing time, so adjust with care!
}

\references{
	This function is an adapted and expanded version of the from the \code{spectrogram()} function in the \href{http://cran.r-project.org/web/packages/phonTools/index.html}{\code{phonTools} package} written by Santiago Barreda.
	It is also available in the following GitHub repository, along with other spectrogram-related functions: \url{https://github.com/usagi5886/dsp}
}

\author{
	Aaron Albin (\url{http://www.aaronalbin.com/})
}

\seealso{
	\code{\link{RichVisualization}}, where a spectrogram generated with this function forms one of the three panels in the plot
}

\examples{
# Create path to sample wave file ('HelloWorld.wav') included in the package
WavePath = paste(R.home("library"),"intonation","HelloWorld.wav",sep="/")

# Import the audio package
require("audio")

# Load the audio into R
SoundObject = load.wave(WavePath) # Function load.wave() comes from package 'audio'

# Play the audio
play(SoundObject) # Function play() comes from package 'audio'
# You should hear a synthesized voice saying "Hello, world".

#####################################
# Three ways to specify audio input #
#####################################

# Option A: Provide the path to the WAV file
Spectrogram(Audio=WavePath)

# Option B: Provide the R-internal object with the audio data
Spectrogram(Audio=SoundObject)

# Option C: Provide a numeric vector with the audio data (and specify SamplingFrequency)
SineWave = sin( seq(from=1,to=2000*(2*pi),length.out=8000) ) # 2000 Hz sine wave
play(SineWave) # High-pitched beep
Spectrogram(Audio=SineWave,SamplingFrequency=8000) # Horizontal bar at 2000 Hz

#################################
# Adjusting analysis parameters #
#################################

# Adjusting analysis window (narrow- vs. wide-band spectrogram)
layout(matrix(1:2,ncol=2))
Spectrogram(SoundObject, WindowLength=5, main="Narrow band (WindowLength = 5 ms)") # The default
Spectrogram(SoundObject, WindowLength=50, main="Wide band (WindowLength = 50 ms)")

# Adjusting the frequency resolution
layout(matrix(1:2,ncol=2))
Spectrogram(SoundObject, FrequencyResolution=4, main="FrequencyResolution=4")
Spectrogram(SoundObject, FrequencyResolution=1, main="FrequencyResolution=1") # Aliased (Too few frequency bins)

# Specifying time steps
layout(matrix(1:2,ncol=2))
Spectrogram(SoundObject, TimeStepSize=1, main="TimeStepSize = 1 ms")
Spectrogram(SoundObject, TimeStepSize=10, main="TimeStepSize = 10 ms") # Aliased (Too few time steps)

# Can also specify number of time steps
layout(matrix(1:2,ncol=2))
Spectrogram(SoundObject, nTimeSteps=400, main="nTimeSteps = 400") # This is the default, hence this is equivalent to simply Spectrogram(SoundObject)
Spectrogram(SoundObject, nTimeSteps=800, main="nTimeSteps = 800")

# Application of pre-emphasis
layout(matrix(1:2,ncol=2))
Spectrogram(SoundObject, Preemphasis=TRUE, main="Preemphasis = TRUE (default)")
Spectrogram(SoundObject, Preemphasis=FALSE, main="Preemphasis = FALSE")

# Decreasing dynamic range
layout(matrix(1:2,ncol=2))
Spectrogram(SoundObject, DynamicRange=70, main="DynamicRange = 70 (default)")
Spectrogram(SoundObject, DynamicRange=50, main="DynamicRange = 50")

# Whether to omit the 0-frequency band (when DynamicRange=NULL)
layout(matrix(1:2,ncol=2))
Spectrogram(SoundObject, DynamicRange=NULL, Omit0Frequency=FALSE, main="Omit0Frequency = FALSE (default)")
Spectrogram(SoundObject, DynamicRange=NULL, Omit0Frequency=TRUE, main="Omit0Frequency = TRUE")

# Changing the window type/parameter
layout(matrix(1:2,ncol=2))
Spectrogram(SoundObject, WindowType="kaiser", WindowParameter=3, Omit0Frequency=FALSE, main="WindowType=Kaiser,\nWindowParameter=3 (default)")
Spectrogram(SoundObject, WindowType="rectangular", main="WindowType = Rectangular")

#######################
# Plotting parameters #
#######################

# Return the spectrogram as a matrix
SpectrogramMatrix = Spectrogram(SoundObject, plot=FALSE)
fix(SpectrogramMatrix) # View the result

# Toggle PlotFast=TRUE (generate plot quickly) vs. PlotFast=FALSE (higher quality image)
layout(matrix(1:2,ncol=2))
Spectrogram(SoundObject, PlotFast=TRUE) # Default
Spectrogram(SoundObject, PlotFast=FALSE)
# The slowdown with the latter is quite noticeable.

# Adding a spectrogram to an existing plot
Spectrogram(SoundObject) # First plot speech soundfile
Spectrogram(SineWave, SamplingFrequency=8000,add=TRUE) # Now add sine wave spectrogram in bottom-left corner
# If the time and frequency domains match between the two files, the second spectrogram will cover the first one (thereby replacing it)

# Other color palettes
layout(matrix(1:2,ncol=2))
Spectrogram(SoundObject, col="alternate", main="'Alternate' color palette")
Spectrogram(SoundObject, col="grayscale", main="Grayscale")

# Specifying custom color palette
Spectrogram(SoundObject, col=c("cyan","white","magenta"), main="Custom palette (cyan-white-magenta)")

# Adjusting axis limits
layout(matrix(1:2,ncol=2))
Spectrogram(SoundObject, xlim=c(100,665), main="x axis = 100-665 ms", TimeStepSize=1) # The word 'hello' by itself
Spectrogram(SoundObject, ylim=c(0,2000), main="y axis = 0-2000 Hz", FrequencyResolution=16) # Low frequencies only

# Axis labels
Spectrogram(SoundObject, xlab = "This is my x axis label", ylab = "This is my y axis label", main="Custom axis labels")

# Other adjustments must be made through a separate call to par()
par(las=1) # Make axis labels always horizontal (in the 'reading direction')
Spectrogram(SoundObject, main="y axis labels are horizontal")
}

\keyword{ aplot }
\keyword{ dplot }
\keyword{ manip }
\keyword{ file }