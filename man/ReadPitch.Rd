\name{ReadPitch}
\alias{ReadPitch}

\title{
	Read a Pitch object into R
}

\description{
	This function takes a Pitch object textfile, reads it into R in a quick and efficient way, and returns the requested information.
}

\usage{
ReadPitch( File, Return = "Frames" )
}

\arguments{
	\item{File}{Path to a Pitch object stored on one's computer. This must be stored in Praat's "text" format (and not "short (text)" or "binary" format). This file path does *not* have to be a full path, and it *can* have spaces in it.}
	\item{Return}{Specifies the kind of information about the Pitch object to be brought into R. See "Details" section below.}
}

\details{
	Three values are accepted for \code{Return}:
	\tabular{ll}{
		\code{Header} \tab All the info from the header (in the same order)\cr
		\code{Frames} \tab Information about each frame (and each frame's highest-ranking candidate)\cr
		\code{Candidates} \tab Information about every candidate in each frame\cr
	}
	The default is \code{"Frames"} since it represents the most common use case.
}

\value{
	\describe{
		\item{If \code{Return="Header"}:}{A list with the following entries: \code{FileStartTime}, \code{FileEndTime}, \code{nFrames}, \code{TimeStep}, \code{FirstTime}, \code{Ceiling}, \code{MaxNumberCandidates}}
		\item{If \code{Return="Frames"}:}{A dataframe with the following columns: \code{Time}, \code{Frequency}, \code{Intensity}, \code{Strength}, \code{nCandidates}}
		\item{If \code{Return="Candidates"}:}{A dataframe with the following columns: \code{nthFrame}, \code{nthCandidate}, \code{Frequency}, \code{Strength}}
		The default is \code{"Frames"} since it represents the most common use case.
	}
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

\seealso{
	This function is called from within \code{\link{RichVisualization}} in order to generate the F0 track in the center panel.
	For creating Pitch objects in the first place, see \code{\link{ToPitch}}.
}

\examples{
# Create path to sample Pitch object textfile ('HelloWorld.Pitch') included in the package
PitchPath = paste(R.home("library"),"intonation","HelloWorld.Pitch",sep="/")

# Return the information contained in the header of the Pitch object
# More specifically, this queries the file's start/end time, the total number of frames, the time step, the timestamp of the first sample, the F0 ceiling, and the maximum number of candidates per frame.
Header = ReadPitch( PitchPath, Return="Header" )
print(Header) # Inspect the results

# Return information about the frames in the Pitch object.
# More specifically, this queries the time, frequency, intensity, strength, and number of candidates in each frame.
Frames = ReadPitch( PitchPath, Return="Frames" )
fix(Frames) # Inspect the results

# Return="Frames" is the default, hence the previous line is equivalent to
Frames = ReadPitch( PitchPath )

# Return information about the F0 candidates in the Pitch object.
# More specifically, this queries which frame each candidate is in as well as the candidate ranking, frequency, and strength of each candidate.
Candidates = ReadPitch( PitchPath, Return="Candidates" )
fix(Candidates) # Inspect the results

}

\keyword{ IO }
\keyword{ file }
