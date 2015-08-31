\name{intonation}

\alias{intonation}

\docType{package}

\title{R package for the linguistic analysis of fundamental frequency (F0) in speech}

\description{
	This package consists of the functions used in \href{http://hdl.handle.net/2022/20345}{Albin (2015)} for various tasks relating to speech \href{https://en.wikipedia.org/wiki/Fundamental_frequency}{fundamental frequency}, including
	\enumerate{
	\item{manipulating Pitch objects from the phonetics software \href{http://www.fon.hum.uva.nl/praat/}{Praat},}
	\item{visualizing F0 data in a way that retains rich information from the acoustic signal, and}
	\item{creating and plotting a schematic quantitative representation ('stylization') of an F0 track.}
	}
}

\details{
	\tabular{ll}{
	Package: \tab intonation\cr
	Type: \tab Package\cr
	Version: \tab 1.0\cr
	Date: \tab 2015-08-31\cr
	License: \tab GNU General Public License (Version 3 or later)\cr
	Imports: \tab audio, PraatR\cr
}
}

\references{
	Albin, A. (2015). \emph{Typologizing native language influence on intonation in a second language: Three transfer phenomena in Japanese EFL learners}. (Doctoral dissertation). Indiana University, Bloomington. \href{http://hdl.handle.net/2022/20345}{http://dx.doi.org/10.5967/K8JW8BSC}
}

\author{
Aaron Albin (\url{http://www.aaronalbin.com/})
}

\note{
	This package has two dependencies: \href{https://cran.r-project.org/web/packages/audio/index.html}{audio} and \href{http://www.aaronalbin.com/praatr/}{PraatR}. To install the latter, follow the steps under "Installation" on the PraatR homepage. (Note, however, that that only function in this package relying on PraatR is \code{\link{ToPitch}}.)
}

\seealso{
In addition to some sample data (\code{\link{HelloWorld}}), the package includes the following functions:

	\tabular{ll}{
		\code{\link{Spectrogram}} \tab Create a spectrogram of an audio file\cr
		\code{\link{ToPitch}} \tab Use Praat to generate a Pitch object\cr
		\code{\link{ReadPitch}} \tab Read a Pitch object into R\cr
		\code{\link{RichVisualization}} \tab Visualize the F0 data in a Pitch object\cr
		\code{\link{Stylize}} \tab Create a 'stylization' (i.e., a schematic representation) of F0 an track\cr
		\code{\link{PlotStylization}} \tab Draw a stylization on an open plot of an F0 track\cr
	}

}

\examples{
# Create paths to the WAV file and Pitch object textfile included in the package
WavePath = paste(R.home("library"),"intonation","HelloWorld.wav",sep="/")
PitchPath = sub(WavePath,pattern=".wav",replacement=".Pitch",fixed=TRUE)

# Draw a spectrogram of the audio data in a soundfile
Spectrogram(WavePath)

\dontrun{
# Create a Pitch object from this soundfile
ToPitch( Input=WavePath, Output=PitchPath, Range=c(69,135) )}

# Read this Pitch object into R
head( ReadPitch( PitchPath ) )

# Plot the F0 data contained therein as a 'rich visualization'
RichVisualization( PitchPath=PitchPath, WavePath=WavePath, 
                   Labels = c("hello","world"), Divisions_ms = c(132,648,1257) )

# Create a stylization of the F0 track
Stylization = Stylize( PitchPath, VertexIndices=c(211,489,1123) )

# Superimpose this stylization on the rich visualization
PlotStylization(Stylization)
}

\keyword{ package }
