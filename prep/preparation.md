---
title: Software installation instructions
---

------------------------------

Please read the appropriate sections below, which give specific instructions for installing and testing the software we will be using.
First follow the instructions for "all users", then those for your specific operating system (OS).

**NB:** If you run into problems, send a note to kingaaictp@gmail.com with a detailed description of the problem you've encountered.
In this message, be **certain** to include:

- the operating system you’re running,
- the version numbers of **R**, **Rstudio**, and **pomp** you’re attempting to install,
- what command you've executed, and
- what error messages you've received.

In particular, it is often easiest to send a screenshot or transcript showing the commands you've entered and the error messages you've received.
In **R**, you can run `Sys.info()` to get a printout of the operating system and software version numbers.

### All users:

**R** and **Rstudio** are free and open-source.
You’ll need at least version 3.2.1 of **R**.
The latest version is 3.2.5, so if you need to update, go ahead and install version 3.2.5.
Source code and binaries are available on CRAN (http://cran.r-project.org).
Install the latest version of **Rstudio** from [rstudio.com](http://www.rstudio.com/products/rstudio/download/).

Once you’ve installed these, open a session in **R** or **Rstudio** and run the following:

```
> update.packages()
> source("http://kingaa.github.io/short-course/prep/packages.R")
> source("http://kingaa.github.io/scripts/pompTest.R")
```

[The `>` is the command prompt; it is not part of the command.
Also, depending on your email client program, you may need to replace the quotation marks with plain keyboard double quotes.]

The first command updates your installed packages.
You may be prompted to specify a CRAN mirror:
choose one geographically near you.
The second command runs a script on my website.
It will install some needed packages if these are not already installed on your system.
The third command will attempt to install **pomp**, the principal **R** package we’ll be using, and will check whether you can work with it.
The installed **pomp** version is more recent than that which is on CRAN.

If the final command fails, try the following:
```
> source("http://kingaa.github.io/scripts/hello.R",echo=TRUE)
```
If this fails to give the "Hello world!" message, you will need to follow the instructions below that correspond to your OS.

### Linux and unix users:

If you have trouble with either script above, make sure you have the GNU compiler collection (GCC) installed on your computer.
Linux distributions typically include this by default but it is not impossible that you have somehow avoided this.

### MacOSX users:

So that you can compile C code and dynamically link it into an **R** session, you will need to make sure you have the Xcode app installed before running the second script above.
This is gratis and can be installed via the App Store or downloaded from [developer.apple.com](https://developer.apple.com/xcode/downloads/).

If the `pompTest.R` script fails because you cannot load **pomp**, try installing it from source.
The easiest way to do this is to use the **devtools** package.
Do
```
install.packages("devtools")
library(devtools)
install_git("kingaa/pomp")
```
If, while trying to install from source, you receive the error,
```
make: gfortran-4.8: No such file or directory
```
or one that otherwise refers to `gfortran`, then it is likely that you do not have the necessary version of **gfortran** installed.
Have a look at [these instructions](http://kingaa.github.io/mac-fortran.html) and contact me at the address above if these don’t work for you.

### Windows users:

You will need the ability to compile C code and dynamically link it into an **R** session.
To do this, you’ll need to install the **Rtools** suite.
[Download the latest version from CRAN](http://cran.r-project.org/bin/windows/Rtools) and install it.

When installing **Rtools**, it is sufficient to choose the “Package authoring installation” option.
Also during the installation, tick the “edit system PATH” box.

Briefly, I had some problems with **Rtools33**, but none with the last frozen version (**Rtools32**).
I also had some difficulties initially with the latest version of **Rstudio** but these went away when I installed version 0.98.1103 ([download here](https://support.rstudio.com/hc/en-us/articles/206569407-Older-Versions-of-RStudio-Desktop)) and now I am having no problems with version **Rstudio** version 0.99.447.

------------------------------

#### [Back to course homepage](../)

------------------------------
