\name{PlotStylization}
\alias{PlotStylization}

\title{
	Draw a stylization on an open plot
}

\description{
	This function takes a Stylization dataframe as input and draws it the information therein (e.g. the vertex locations and transition shapes) to draw the stylization in R's currently active window.
}

\usage{
PlotStylization( Stylization )
}

\arguments{
	\item{Stylization}{A stylization dataframe created with the \code{\link{Stylize}} function.}
}

\details{
	It is assumed that the currently active window contains an F0 track (x axis = time, y axis = F0) representing the same soundfile that the stylization is based on. This F0 track plot can be created with the \code{\link{RichVisualization}} function in this package or any other means. The stylization will be superimposed on top of this F0 track in the form of points and lines.
}

\value{
	None. This function is used for its side-effect - adding points and lines to the currently open window.
}

\references{
	This function is referenced on page 67 of
	\itemize{
		\item{Albin, A. (2015). \emph{Typologizing native language influence on intonation in a second language: Three transfer phenomena in Japanese EFL learners}. (Doctoral dissertation). Indiana University, Bloomington. \href{http://hdl.handle.net/2022/20345}{http://dx.doi.org/10.5967/K8JW8BSC}}
	}
}

\author{
	Aaron Albin (\url{http://www.aaronalbin.com/})
}

\seealso{
	\code{\link{Stylize}} for creating the stylization dataframe in the first place. An F0 track can be produced with \code{\link{RichVisualization}}.
}

\examples{
# Create paths to the WAV file and Pitch object textfile included in the package
WavePath = paste(R.home("library"),"intonation","HelloWorld.wav",sep="/")
PitchPath = sub(WavePath,pattern=".wav",replacement=".Pitch",fixed=TRUE)

# Generate a RichVisualization plot, including the F0 track that should be stylized
RichVisualization( PitchPath=PitchPath, WavePath=WavePath, 
                   Labels = c("hello","world"), Divisions_ms = c(132,648,1257) )

# Create the stylization dataframe and store it in a variable
Stylization = Stylize(PitchPath, VertexIndices=c(211,489,1123))

# Plot the stylization
PlotStylization(Stylization)
}

\keyword{ aplot }
