# Open Online Introduction to R Course

## General Information

<table>
<tr>
   <td>Course Dates</td>
   <td>16-18 September, 2020 + 2 Extra Days</td>
</tr>
<tr>
   <td>Course Times</td>
   <td>17:00 - 22:00 CEST (<a href="https://www.timeanddate.com/worldclock/fixedtime.html?iso=20200916T17&p1=1307">check local time</a>)</td>
</tr>
<tr>
   <td>Course Location</td>
   <td>online / <a href="https://www.twitch.tv/wviechtb">live stream</a></td>
</tr>
<tr>
   <td>Registration</td>
   <td>not necessary</td>
</tr>
<tr>
   <td>Course Fee</td>
   <td>none (this course is free!)</td>
</tr>
</table>

## General Information

R is a programming language and software environment for carrying out computations, manipulating and analyzing data, and creating various types of plots and graphics (see the [R project website](https://www.r-project.org/) for more info). R has become the 'lingua franca of statistics' and the software of choice for analyzing data in various disciplines. However, for many researchers, getting up and running with R remains a hurdle due to the command-driven nature of the software. The purpose of this course is to lay the necessary foundation for becoming a proficient R user.

In this course, we will cover:

* a bit of history and the development of R
* how to use and interact with R
* basic data structures
* data import and export
* data inspection and manipulation
* methods for graphing data
* t-tests and analysis of (co)variance
* correlations and linear regression
* categorical data analysis and logistic regression
* add-on packages (how to find, install, and work with them)
* how/where to obtain help when you get stuck
* basic programming structures (e.g., loops, if-else statements)
* writing documents with R Markdown

**Note:** When discussing statistical methods/models, emphasis will be on the general syntax as used in R and less on the statistical details of the various procedures (i.e., this is not a 'stats' course, it is a 'how to do stats with R' course). Hence, some familiarity with basic statistical concepts and methods is helpful when following the course.

The course is aimed at researchers, (Master and PhD level) students, data analysts/scientist, and essentially anybody interested in learning how to work with R.

## Course Schedule

**Note**: This schedule is tentative. Given the dynamics of a live and interactive course, the times are only approximations. Also, shorter breaks are not explicitly indicated in the schedule below, but will happen throughout the days as needed.

#### Day 1

| Time        | Topic                |
| :---------- | :------------------- |
| 17:00-18:00 | Introductory Lecture |
| 18:00-19:00 | Interacting with R |
| 19:00-20:00 | Basic Data Structures |
| 20:00-20:30 | Break |
| 20:30-22:00 | Working with Data Frames |

#### Day 2

| Time        | Topic                |
| :---------- | :------------------- |
| 17:00-19:00 | Importing and Inspecting Data |
| 19:00-20:00 | Basic and Some Advanced Plotting |
| 20:00-20:30 | Break |
| 20:30-22:00 | Basic and Some Advanced Plotting |

#### Day 3

| Time        | Topic                |
| :---------- | :------------------- |
| 17:00-20:00 | Statistics with Continuous Outcomes |
| 20:00-20:30 | Break |
| 20:30-22:00 | Statistics with Continuous Outcomes |

#### Extra Day 1
| Time        | Topic                |
| :---------- | :------------------- |
| 17:00-18:00 | Recap of Previous Days  |
| 18:00-19:30 | Statistics with Categorical Outcomes |
| 19:30-20:30 | Break |
| 20:00-21:00 | Working with R Packages |

#### Extra Day 2
| Time        | Topic                |
| :---------- | :------------------- |
| 17:00-19:00 | Programming Structures |
| 19:00-19:30 | Break |
| 19:30-21:00 | R Markdown |

## Course Format

The course will be taught online as a live stream via the streaming platform Twitch. Once the stream goes live (around 16:45 on each course day), simply go to [this link](https://www.twitch.tv/wviechtb) to start watching.

After an introductory lecture, the format of the course will be quite simple: I will provide you with R code that we will then go through step-by-step. Along the way, I'll explain how things work and answer questions as necessary. The code will be posted on the [course website](http://www.wvbauer.com/doku.php/course_oor#r_code) (and on [GitHub](https://github.com/wviechtb/course_oor/tree/master/code) and [GitLab](https://gitlab.com/wviechtb/course_oor/-/tree/master/code)). I provide the code at several locations to create some redundancy in case one of the links becomes inaccessible.

## How to Prepare for the Course

You will need a computer with the current version of R installed. You can download R from the [Comprehensive R Archive Network](https://cran.r-project.org/) (CRAN). Follow the appropriate "Download R" link depending on your operating system (OS) and follow the instructions for downloading and installing R. If you already have R installed, please check that it is the current version (you can check what the 'latest release' of R is by going to [CRAN](https://cran.r-project.org/) and then compare this with the version shown when you start R). If not, please update.

Although not strictly necessary, it will also be useful to install an integrated development environment (IDE) for R. A popular choice these days is RStudio. So, unless you already have a different setup, download the appropriate installer of RStudio for your OS from [here](https://rstudio.com/products/rstudio/download/#download) and install in the usual manner.

You do not need to have a Twitch account to watch the stream, but if you would like to post comments or questions via the chat (see below), then you do need to be signed in. To create an account, go to [Twitch](https://www.twitch.tv/), click on "Sign Up", and follow the instructions. Make sure you also verify your email address as part of the registration procedure (otherwise you cannot use the chat).

You might also want to think a bit ahead of time how you will arrange your desktop while following the course. You will want to have both your browser (for following the live stream) and R/RStudio open at the same time and ideally put them side-by-side (otherwise, you will have to switch back and forth between these windows, which will become tedious rather quickly). So, unless you have a large monitor, two computers/monitors (i.e., one for the stream, one for R/RStudio) would be ideal.

~~Just as an idea, you could also consider following the course together with other people as a group and set up a computer/projector to show the stream, while everybody can work on their own computer/laptop to follow along~~ (scratch that; this is not a good idea at the moment).

## Chat (Purpose and Rules)

Usually, I teach courses 'in person' and there are various benefits from being in the same room as the course participants. For one thing, if I see a lot of confused faces, it tells me to slow down or reexplain things. The chat will have to replace this form of communication. If something is unclear, just let me know via the chat.

If possible, I will also be happy to answer 'but how do I do this or that?' type questions (assuming they relate to what is being covered in the course at that moment). However, if there are too many questions of this type at the same time, the chat will become unusable, so please consider carefully whether it is appropriate to ask such questions at particular moments.

Note that there is a slight delay when live streaming (between a few and sometimes up to 10-20 seconds), so keep that in mind when asking questions. This can also make 'back-and-forth' questioning difficult (e.g., if I have to ask for some clarification about your question).

Any type of harassment or hateful conduct, inappropriate commenting, or disruptive behavior will not be tolerated and will lead to temporary chat timeouts or to being banned from the chat permanently.

## Other Things / Notes / FAQs

Some other notes and frequently asked questions that may come up:

* During the course, we will make use of a number of add-on packages for R. Ideally, you should install these packages before the course (then you don't have to bother with installing packages during the course). You should be able to do so by running the code provided [here](https://gist.githubusercontent.com/wviechtb/eb125cb6f7c706982f72e5872312fbba/raw/1fec0fea6216d162319822f31d7d74bfcad52e07/install_packages.r) in R (just open up R and copy-paste the `install.packages()` lines one by one).

* In the 'in person' courses that I teach, I often end up troubleshooting some general computer problems for one or multiple course participants. I will not be able to do this in this course. I also cannot provide statistical support or answer questions you have about your own data analysis needs.

* I reserve the option to end the course at any point (e.g., due to low attendance, technical problems). Also, depending on how things go (e.g., if there are technical problems, lots of questions), I might not be able to cover all topics planned.

* If, for whatever reason, the stream goes down (and I cannot reconnect and get it running again), I will post a note at the top of this page.

* I cannot provide 'course certificates' or some other form of certification that you have participated in the course.

* The stream will not be recorded. The whole point is that this is an interactive live stream, which sets it apart from online courses that use pre-recorded videos or any of the thousands of YouTube videos that teach R. If I wanted to create the latter, I would script the entire video (which is not compatible with the dynamics of an interactive live stream) and do a lot of post-processing. If you are looking for such online courses, you could look into those that are offered at [Coursera](https://www.coursera.org/browse/data-science/data-analysis), [edX](https://www.edx.org/course/subject/data-science), and similar platforms.

* The stream will not be recorded. The whole point is that this is an interactive live stream, which sets it apart from online courses that use pre-recorded videos or any of the thousands of YouTube videos that teach R. If I wanted to create the latter, I would script the entire video (which is not compatible with the dynamics of an interactive live stream) and do a lot of post-processing.

* If you have to miss parts of the course (e.g., due to other obligations), you can still obtain the materials and work through them on your own pace. The same is of course also possible if you have to miss the entire course.

* If the stream lags/stutters, try reloading the stream (just refresh the page) or reduce the stream resolution (click on the little gear icon at the bottom right of the stream window, select 'Quality', and try 720p or 420p; you could also try disabling 'Low Latency' under 'Advanced'). Note that the option to reduce the stream resolution might not be available (which is not something I have control over, although there are ways to make this option available once the stream is running for a bit).

* For people affiliated with Maastricht University: You might want to consider following the [Introduction to R Course](https://www.maastrichtuniversity.nl/education/course/introduction-r) I teach for the university (high attendance / interest in this course shows the university the importance of keeping the course in its curriculum and you also get the benefits from 'in person' interactions).

## Relevant Links

The following are relevant links for the course itself:

* [Stream at Twitch](https://www.twitch.tv/wviechtb) (where to go to watch the stream once it goes live)
* [Chat Only](https://www.twitch.tv/popout/wviechtb/chat) (in case you have another computer showing the stream, but want to ask questions)
* [Course Website](http://www.wvbauer.com/doku.php/course_oor) (the 'official' course website, but this repo contains the same materials)
* [Pre-Course Questionnaire](https://forms.gle/VgrFRyzP252VicBa9) (a short questionnaire to obtain a bit more information about your background)

The following are relevant links in general:

* [R Project Website](https://www.r-project.org/)
* [CRAN Website](https://cran.r-project.org/)
* [RStudio Website](https://www.rstudio.com/)
* [Cross Validated](https://stats.stackexchange.com/tour)
* [Stack Overflow](https://stackoverflow.com/tour)

## License

The contents of this repo are licensed under the following license: [CC Attribution-Noncommercial-Share Alike 4.0 International](http://creativecommons.org/licenses/by-nc-sa/4.0/).
