\name{HelloWorld}
\alias{HelloWorld}

\docType{data}

\title{
	Synthesized voice saying "hello world", plus accompanying Pitch object and TextGrid
}

\description{
	\itemize{
		\item{
			The WAV file was created using the \href{http://tts.speech.cs.cmu.edu:8083/}{Flite English Synthesis Demo} by selecting \code{cmu_us_rms.flitevox} under "Choose voice", entering the text \code{Hello, world!}, and hitting "Download!".
		}
		\item{
			The Pitch object was created in Praat with the "To Pitch..." command with a time step of 1 millisecond (0.001 seconds) and a F0 range of 69-135 Hertz.
		}
		\item{
			The TextGrid was also created in Praat and has one interval tier named "words".
		}
	}
}


\details{
	The sole tier ("word") of the TextGrid contains the labels \code{"<SIL>"}, \code{"hello"}, \code{"world"}, and \code{"<SIL>"}. The boundaries occur at roughly 0.000, 0.132, 0.648, 1.257, and 1.355 seconds.
}

\source{
	The synthesis used to create the WAV file was created by \href{http://www.festvox.org/flite/}{Flite} using a demo built with \href{https://github.com/happyalu/goflite}{GoFlite}.
}

\references{
	Black, Alan W., &  Lenzo, Kevin A. (2001). \href{http://www.isca-speech.org/archive_open/ssw4/ssw4_204.html}{Flite: a small fast run-time synthesis engine}. In \emph{Proceedings from the 4th ISCA Tutorial and Research Workshop (ITRW) on Speech Synthesis} (paper #204). Perthshire, Scotland: International Speech Communication Association (ISCA).
}

\examples{
# Create paths to all three files
WavePath = paste(R.home("library"),"intonation","HelloWorld.wav",sep="/")
PitchPath = sub(WavePath,pattern=".wav",replacement=".Pitch",fixed=TRUE)
TextGridPath = sub(WavePath,pattern=".wav",replacement=".TextGrid",fixed=TRUE)

# Read the WAV file into R using load.wave() (from the 'audio' package)
SoundObject = load.wave(WavePath)

# Read the Pitch object into R using ToPitch() (another function in this package)
Frames = ReadPitch( PitchPath )
head(ReadPitch) # Inspect the results

# Import the TextGrid (in unaltered form) with the base-R function 'readLines()'
readLines(TextGridPath)
}

\keyword{datasets}
