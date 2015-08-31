\name{Stylize}
\alias{Stylize}

\title{
	Create a stylized F0 contour
}

\description{
	This function takes a set of vertices (F0 targets), uses this information to compute the shape of a schematic contour fit to the raw F0 data, and returns a dataframe describing the makeup of this contour.
}

\usage{
Stylize( PitchPath,
         Message = TRUE,
         nVertices,
         WavePath,
         PlayDuration_ms = 1000,
         ZeroPad = TRUE,
         ClickSound,
         VertexIndices,
         nSeeds = 10
)
}

\arguments{
	\item{PitchPath}{Path to a Pitch object textfile stored in Praat's (long) 'text' format. If stylizing interactively, it is assumed that the F0 data in this Pitch object is the same data as that displayed in the currently open plot.}
	\item{Message}{If \code{TRUE}, the message "Please select the contour vertices." is displayed in the R console (to make it clear that user input is being requested).}
	\item{nVertices}{If left unspecified, when done selecting vertices (i.e. clicking F0 points), the user must right-click and select "Stop". Instead, \code{nVertices} can be set to an integer specifying up front how many vertices will be selected (after which the interactive selection process is automatically terminated).}
	\item{WavePath}{Path to a WAV file one one's computer containing the audio from which the Pitch object (at \code{PitchPath}) was derived. If \code{WavePath} is specified, after each click during the stylization process, a short clip of that soundfile is played.}
	\item{PlayDuration_ms}{The duration (in milliseconds) of the soundfile clip that should be played. Defaults to 1000 ms, i.e. 1 second.}
	\item{ZeroPad}{Logical variable indicating whether a half-second of silence should be appended to the end of the soundfile clip when played. (This can avoid problems in audio playback on some systems.)}
	\item{ClickSound}{Logical variable indicating whether a bell or other similar sound (depending on the operating system) should play after each click. If \code{WavePath} is provided, defaults to \code{FALSE}. If not, defaults to \code{TRUE}.}
	\item{VertexIndices}{A set of integers (in ascending order) indicating which frames in the Pitch object correspond to the vertices. The F0 values in these frames must be non-\code{NA}.}
	\item{nSeeds}{Number of seeds for exploring the search space. For details, see section "nSeeds" below.}
}

\details{
	This function takes as input information about the F0 turning points ('vertices') one hypothesizes to underlie the F0 track. The function allows for this information to be provided in two ways:
	\enumerate{
		\item{Interactively, by asking the user to select points in a currently open F0 track plot (x axis = time, y axis = F0)}
		\item{Non-interactively, by providing (via the \code{VertexIndices} argument) a vector indicating which frames in the Pitch object correspond to the vertices}
	}
	If \code{VertexIndices} is provided, the interactive vertex-selection mode will be skipped, hence the arguments \code{WavePath}, \code{PlayDuration_ms}, \code{ZeroPad}, \code{Message}, and \code{nVertices} are ignored (with a warning).
}

\value{
	As output, a specially structured dataframe is returned containing all the relevant information about the stylized F0 contour. The row numbering for this dataframe begins at 0 (since the first vertex in a contour is merely a 'starting value' and, as such, has no associated F0 transition). The dataframe has the following columns:
	\tabular{ll}{
		\code{Index} \tab the index for this vertex (i.e. which frame in the Pitch object corresponds to this vertex)\cr
		\code{F0} \tab The F0 value (in Hertz) for this vertex\cr
		\code{Time_ms} \tab The timestamp (in millseconds) of this vertex\cr
		\code{Threshold} \tab The threshold value of the F0 transition leading up to this vertex\cr
		\code{Gradience} \tab The gradience value of the F0 transition leading up to this vertex\cr
		\code{MAD} \tab The intensity-weighted median absolute deviation, describing the goodness of fit of the stylization to the raw data (with smaller values indicating better fit)\cr
		\code{PercentVoiced} \tab A number between 0 (0\%) and 1 (100\%) indicating the proportion of the frames in the transition leading up to this vertex that are voiced\cr
		}
	For further information on threshold and gradience, see section 3.4.3 on p.79ff of Albin (2015).
}

\section{nSeeds}{
	The argument \code{nSeeds} specifies the number of seeds (i.e. initialization candidates) for exploring the search space during the non-linear optimization process where the threshold and gradience parameters are determined. Any number between 1 and 100 is valid. Decreasing \code{nSeeds} makes the code run faster but makes it less guaranteed that an optimal fit will be obtained. \code{nSeeds} defaults to 10, i.e. the algorithm uses the top 10\% of the 100 candidates in the 10x10 grid described on p.84-86 of Albin (2015). Note that, since \code{nSeeds} is specified at the level of the \code{Stylize()} function as a whole, all F0 transitions in the contour will have the same nSeeds applied in the fitting process.
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

\note{
	If you ever see a menu with the options "Copy as metafile", "Copy as bitmap", "Save as metafile...", "Save as postscript...", "Stay on top", and "Print..." (or similar, depending on your R installation), you have clicked too soon. In this situation, simply hit the escape key to exit the menu and then make your next click.
}

\seealso{
	Once a stylization has been created with this function, use \code{\link{PlotStylization}} to plot it. One way to create an F0 track to stylize in the first place is with \code{\link{RichVisualization}}.
}

\examples{
# Create paths to the WAV file and Pitch object textfile included in the package
WavePath = paste(R.home("library"),"intonation","HelloWorld.wav",sep="/")
PitchPath = sub(WavePath,pattern=".wav",replacement=".Pitch",fixed=TRUE)

# Generate a RichVisualization plot, including the F0 track that should be stylized
RichVisualization( PitchPath=PitchPath, WavePath=WavePath, 
                   Labels = c("hello","world"), Divisions_ms = c(132,648,1257) )

\dontrun{

# Run interactive mode, with all defaults (specifying only 'PitchPath' - the only required argument to this function)
# Click a few points (e.g. at the beginning, middle, and end of the contour), and then right-click and select "Stop"
Stylize(PitchPath)

# Turn off the "Please select the contour vertices." message
Stylize(PitchPath=PitchPath, Message=FALSE)

# Specify up front that only 3 vertices are to be selected
Stylize(PitchPath=PitchPath, nVertices=3)
# This time you don't have to right-click and select stop (since the interactivity is terminated automatically)

# Turn off the click sound
Stylize(PitchPath=PitchPath, ClickSound=FALSE)

# Interactive mode with audio feedback:
Stylize(PitchPath=PitchPath, WavePath=WavePath)
# Note that no click sound plays

# Make the play duration shorter
Stylize(PitchPath=PitchPath, WavePath=WavePath, PlayDuration_ms=500)

# Turn off zero padding:
Stylize(PitchPath=PitchPath, WavePath=WavePath, ZeroPad=FALSE)
}

# Non-interactive mode (by specifying VertexIndices up front)
Stylize(PitchPath, VertexIndices=c(211,489,1123))

# Speed up the processing of the code by reducing the number of seeds for the optimization algorithm
Stylize(PitchPath, VertexIndices=c(211,489,1123), nSeeds=3)

}

\keyword{ dplot }
\keyword{ iplot }
\keyword{ optimize }
