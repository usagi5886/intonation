# intonation

### [R](https://www.r-project.org/) package for the linguistic analysis of fundamental frequency (F0) in speech

Version 1.1 (July 12, 2016)

*[Aaron Albin](http://www.aaronalbin.com/)*

## What this package does

This package consists of the functions used in [Albin (2015)](http://hdl.handle.net/2022/20345) for various tasks relating to speech [fundamental frequency](https://en.wikipedia.org/wiki/Fundamental_frequency) (F0), including:

1. manipulating Pitch objects from the phonetics software [Praat](http://www.fon.hum.uva.nl/praat/),
2. visualizing F0 data in a way that retains rich information from the acoustic signal, and
3. creating and plotting a schematic quantitative representation ('stylization') of an F0 track.

This package is referenced on page 52 of:

> Albin, A. (2015). *Typologizing native language influence on intonation in a second language: Three transfer phenomena in Japanese EFL learners*. (Doctoral dissertation). Indiana University, Bloomington.[http://dx.doi.org/10.5967/K8JW8BSC](http://hdl.handle.net/2022/20345)

The [data used in the dissertation](http://hdl.handle.net/2022/20346) are also available for download.


While at present the contents of this package are limited to the functions from the dissertation, it is anticipated that this package will grow over time to incorporate a variety of other intonation-related functions. As such, other prosody researchers are more than welcome to contribute their own R code to this package. (See the [author's homepage](http://www.aaronalbin.com/) for contact information.)

## Installation

This package has two other package dependencies you must install first.

1. First, download the package [audio](https://cran.r-project.org/web/packages/audio/index.html) from CRAN, e.g. with:
    install.packages("audio").

2. Second, install PraatR by going to the [PraatR homepage](http://www.aaronalbin.com/praatr/) and following the steps under "Installation".

Now you can install this package by following these steps:

1. Click on the "Download ZIP" button on the right side of this page and save the .zip file somewhere on your computer.

2. Unzip the .zip file to somewhere convenient (e.g. your desktop).

 * After this step, the .zip file is no longer needed and can be deleted.

3. Open R.

 * If you are running Windows, you must open R as an administrator. To do so, go to the icon for R on your start menu ('start screen' in Windows 8) or desktop, right-click it, and select "Run as administrator". Click "Yes" to the User Account Control window that pops up.

4. Find the path to the the unzipped folder from step 2. If you unzipped to your desktop, the path will be something like the following:

 - **Windows**: "C:/Users/MyUsername/Desktop/intonation-master/"

 - **Mac**: "/Users/MyUsername/Desktop/intonation-master/"

5. Run the following line of code, adjusting the path for the *pkgs* argument as needed so it points to path you determined in Step 4.

        install.packages(pkgs="(path from Step 4)", repos=NULL, type="source")

If everything works correctly, you should see something like the following in the R console:

    * installing *source* package 'intonation' ...
    ** R
    ** inst
    ** preparing package for lazy loading
    ** help
    *** installing help indices
    ** building package indices
    ** testing if installed package can be loaded
    *** arch - i386
    *** arch - x64
    * DONE (intonation)

Once installed, type *?intonation* at the R console to pull up the help file for the package, which acts as a table of contents directing you to help files for other specific functions. You can also type *example("intonation")* to see some examples of what the package can do.

## Contents

In addition to some sample data ([HelloWorld](https://github.com/usagi5886/intonation/blob/master/inst)), the package includes the following functions

 - [Spectrogram()](https://github.com/usagi5886/intonation/blob/master/R/Spectrogram.r): Create a spectrogram of an audio file
 - [F0RangeFinder()](https://github.com/usagi5886/intonation/blob/master/R/F0RangeFinder.r): Cycle through each soundfile in a folder and determine F0 range for each
 - [ToPitch()](https://github.com/usagi5886/intonation/blob/master/R/ToPitch.r): Use Praat to generate a Pitch object
 - [ReadPitch()](https://github.com/usagi5886/intonation/blob/master/R/ReadPitch.r): Read a Pitch object into R
 - [RichVisualization()](https://github.com/usagi5886/intonation/blob/master/R/RichVisualization.r): Visualize the F0 data in a Pitch object
 - [Stylize()](https://github.com/usagi5886/intonation/blob/master/R/Stylize.r): Create a 'stylization' (i.e., a schematic representation) of F0 an track
 - [PlotStylization()](https://github.com/usagi5886/intonation/blob/master/R/PlotStylization.r): Draw a stylization on an open plot of an F0 track

Each of these has a help file included in the package. Thus, for example, you can type *?Stylize* to find out more information about how to use the Stylize() function.

## Usage

R documentation 'help files' are available for every function, but the following gives as a rough idea about what the syntax for these functions looks like:

    # Draw a spectrogram of the audio data in a soundfile
    Spectrogram(WavePath)

    # Quickly determine F0 range for every soundfile in one folder (WaveFolder)
    # Save the resulting pitch-related files in another folder (PitchFolder)
	F0Ranges = F0RangeFinder( WaveFolder="C:/Wave/", PitchFolder="C:/Pitch/")

    # Use an F0 range thus determined to create a Pitch object from this soundfile
    ToPitch( Input=WavePath, Output=PitchPath, Range=c(69,135) )

    # Read this Pitch object into R
    ReadPitch( PitchPath )

    # Plot the F0 data contained therein as a 'rich visualization'
    RichVisualization( PitchPath=PitchPath, WavePath=WavePath, Labels = c("hello","world"), Divisions_ms = c(132,648,1257) )

    # Create a stylization of the F0 track
    Stylization = Stylize( PitchPath, VertexIndices=c(211,489,1123) )

    # Superimpose this stylization on the rich visualization
    PlotStylization(Stylization)

## License

This package is released under the GNU General Public License:

> This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

> This program is distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchantability or fitness for a particular purpose.  See the GNU General Public License for more details.

> To receive a copy of the GNU General Public License, see: [http://www.gnu.org/licenses/](http://www.gnu.org/licenses/)
