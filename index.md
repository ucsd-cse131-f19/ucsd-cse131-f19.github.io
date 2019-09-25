---
layout: page
title: "Compiler Construction – CSE 131 S18"
doodle: "/doodle.png"
---

# Compiler Construction

**This site is under revision -- this notice will be removed when contents are finalized.**

<p>
<a href="https://jpolitz.github.io">Joe Gibbs Politz</a> (Instructor)
</p>

<p>
<a href="#basics">Basics</a> -
<a href="#schedule">Schedule</a> -
<a href="#staff">Staff &amp; Resources</a> -
<a href="#grading">Grading</a> -
<a href="#policies">Policies</a>
</p>

In this course, we'll explore the implementation of **compilers**: programs that
transform source programs into other useful, executable forms. This will
include understanding syntax and its structure, checking for and representing
errors in programs, writing programs that generate code, and the interaction
of generated code with a runtime system.

We will explore these topics interactively in lecure, you will implement
an increasingly sophisticated series of compilers throughout the course to
learn how different language features are compiled, and you will think
through design challenges based on what you learn from implementation.

This web page serves as the main source of announcements and resources for the
course, as well as the syllabus.

<a id="basics">
## Basics

- Lecture: Center 109, 9:30am Tue/Thu
- Discussion: Center 212, 8am F **Some attendance required for tests**
- **Tests (in Discussion Section)**: Oct 11, Nov 1, Nov 22
- **Final**: Dec 12, 8am-11am, Location TBA

- Podcasts: <a href="https://podcast.ucsd.edu/podcasts/default.aspx?PodcastId=4931">https://podcast.ucsd.edu/podcasts/default.aspx?PodcastId=4931</a>
- Piazza: <a href="https://piazza.com/class/jfh8ukqgp5h521">https://piazza.com/class/jfh8ukqgp5h521</a>
- Gradescope: <a href="https://www.gradescope.com">https://www.gradescope.com</a> will be used for submissions (instructions will accompany the first programming assignment)
- Textbook/readings: There's no official textbook, but I'll link to different
  online resources for you to read to supplement lecture. Versions of this
  course have been taught at several universities, so sometimes I'll link to
  those instructor's materials.

<a id="schedule">
## Schedule

The schedule below outlines topics, due dates, and links to assignments. In a
typical week, by *Tuesday before class* all due dates, readings, and notable
events in the course until the following week will be posted here. So if you
check the schedule at the beginning of the week, you'll know when all reading
quizzes, programming assignments, etc. will be due. We will often have the
schedule confirmed more than a week out, but we'll always be at least a week
ahead. The schedule of lecture topics might change slightly, but I post a
general plan so you can know roughly where we are headed.

(The first week is an exception; we'll get everything you need for the first
week out by Thursday evening.)

<iframe width="125%" height="500px" src="https://docs.google.com/spreadsheets/d/e/2PACX-1vRhE-LXEqDjFYCTHjXfm3RmaJSY3zhlxihkzo1KCfGof-vm9CtnG3jxS2HumYUIVszN_LDrvq5bAFxT/pubhtml?gid=1954416302&amp;single=true&amp;widget=true&amp;headers=false"></iframe>

<a id="staff">
## Staff & Resources

### Office Hours
Office hours are concentrated on Friday, Monday, and Tuesday, since most
assignments are due Wednesday. Please check the calendar before you come in
case there have been any changes. When you come to the office hour, we may
ask you to put your name in the queue using the whiteboard. Read the
description about [collaboration below](FILL) for some context about office
hours.

FILL Calendar

### Useful Resources

- [OCaml Website](http://ocaml.org/)
- [List of OCaml Tutorials](http://ocaml.org/learn/tutorials/)
- [OCaml Tutorial](http://mirror.ocamlcore.org/ocaml-tutorial.org/)
- [An Incremental Approach to Compiler Construction](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf)

<a id="grading">
## Grading

Your grade will be calculated from:

- 5% participation in class
  - There are 20 lectures, you must attend 10 for full credit
  - Each week both lectures will have clicker questions. You get
    credit for each session where you answer at least half of the questions.
  - For full credit, you need to attend (at least) 10 lectures. For each
    lecture fewer than 10 you attend, you lose one of the participation
    points, minimum 0 (so if you only attend 8 you get 3 participation
    points, if you attend 2 lectures, you get 0 participation points)
  - Clicker scores can be checked [here (coming soon)]
- 5% review quizzes
  - Each week there will be an online review quiz, you get full review quiz
    credit for getting at least half the questions right
  - Quiz scores can be checked [here (coming soon)]
- 50% programming assignments (8 total)
  - PA0 **open collab** (warmup): 2%
  - PA1 **closed collab** (let and arithmetic): 4%
  - PA2 **open collab** (tagged values): 4%
  - PA3 **closed collab** (functions): 8%
  - PA4 **open collab** (types): 6%
  - PA5 **open collab** (using heap allocated data): 6%
  - PA6 **closed collab** (implementing heap alloc): 8%
  - PA7 **open collab** (garbage collection): 6%
  - PA8 **open collab** (projects): 6%
- 40% exams
  - 16% for best 2 of 3 tests taken in discussion section
  - 24% final exam
    - You must pass the final exam to pass the course
    - The final exam will allow for some amount of make-up points for the
    tests (details to come)

<a id="policies">
## Policies

### Late Work

Late work is generally not accepted, because often we'll release partial or
full solutions immediately following the deadline for an assignment.

### Regrades

Mistakes occur in grading. Once grades are posted for an assignment, we will
allow a short period for you to request a fix (announced along with grade
release). If you don't make a request in the given period, the grade you were
initially given is final.

### Exams

You are not allowed any study aids on in-class exams, aside from those
pertaining to university-approved accommodations. References will be provided
along with exams to avoid unnecessary memorization.

You cannot discuss the content of exams with others in the course until grades
have been released for that exam.

### Programming

In your professional programming life, some of your work will be highly
collaborative with lots of expert advice available from senior developers and
from sites like StackOverflow. This is a common case in many Web-focused
companies, in academia, and on open-source projects. It’s a great way to get
exposed to new techniques, share knowledge, and generally enjoy teamwork. In
contrast, some of your work will involve figuring out programming problems on
your own, where you are the first person to encounter an issue, or the first
person to try using a new library in the context of your application. You
should get experience in both types of situations; we might call the former
kind of process **open to collaboration** and the latter **closed to
collaboration**.

In terms of courses, this split also makes sense. Programming assignments
serve (at least) two roles. First and foremost, they are a mechanism for you
to learn! By directly applying the techniques and skills we discuss in class,
you get practice and become a better programmer. Second, they are an
assessment mechanism – as instructional staff we use them to evaluate your
understanding of concepts as demonstrated by your programs. Open
collaboration can reduce frustration while learning and give you chances to
enjoy collaboration and lots of help, but may not let us accurately evaluate
your understanding. Closed assignments are an opportunity for you to
demonstrate what you know by way of programming (and some of the frustration
of working through a problem on your own is _healthy_ frustration).

There are two types of assignments in this course:

- **Open collaboration** assignments, for which you can talk to anyone else in the
  course, post snippets of code on Piazza, get lots of help from TAs, and
  generally come up with solutions collaboratively. TAs will be happy to look
  at your code and suggest fixes, along with explaining them. There are a few
  restrictions:
  - Any code that you didn't write must be cited in the README file that goes
    along with your submission
      - **Example:** On an open collaboration assignment, you and another
        student chat online about the solution, you figure out a particular
        helper method together. Your README should say “The FOO function was
        developed in collaboration with Firstname Lastname”
      - **Example:** On an open collaboration assignment, a student posts the
        compilation strategy they used to handle a type of expression you were
        struggling with. Your README should say “I used the code from
        https://piazza.com/class/id-of-post”
  - Anyone you work with in-person must be noted in your README
      - **Example:** You and another student sit next to each other in the lab,
        and point out mistakes and errors to one another as you work through
        the assignment. As a result, your solutions are substantially similar.
        Your README should say “I collaborated with Firstname Lastname to
        develop my solution.”
  - You cannot share an entire repository of code or paste an entire solution
    into a message board. Keep snippets to reasonable, descriptive chunks of
    code; think a dozen lines or so to get the point across.
  - You still _cannot_ use code that you find online, or get assistance or code
    from students outside of this offering of the class. All the code that is
    handed in should be developed by you or someone in the class.

- **Closed collaboration** assignments, where you cannot collaborate with others.
  You can ask clarification questions as private posts on Piazza or of TAs.
  However, TAs will not look at your code or comment on it. Lab/office hours
  these weeks are for conceptual questions or for questions about past
  assignments only, no code assistance. On these assignments:
    - You cannot look at or use anyone else's code
    - You cannot discuss the assignment with other students
    - You cannot post publicly about the assignment on Piazza (or on social
      media or other forums). Of course, you can still post questions about
      material from lecture on Piazza!
    - All of the examples in the open collaboration section above would be
      academic integrity violations on a closed collaboration assignment

Programming assignments will explicitly list whether they are open or closed
collaboration.

You should be familiar with [the UCSD
guidelines](http://senate.ucsd.edu/Operating-Procedures/Senate-Manual/Appendices/2)
on academic integrity as well.

### Diversity and Inclusion

We are committed to fostering a learning environment for this course that
supports a diversity of thoughts, perspectives and experiences, and respects
your identities (including race, ethnicity, heritage, gender, sex, class,
sexuality, religion, ability, age, educational background, etc.).  Our goal is
to create a diverse and inclusive learning environment where all students feel
comfortable and can thrive.

Our instructional staff will make a concerted effort to be welcoming and
inclusive to the wide diversity of students in this course.  If there is a way
we can make you feel more included please let one of the course staff know,
either in person, via email/discussion board, or even in a note under the door.
Our learning about diverse perspectives and identities is an ongoing process,
and we welcome your perspectives and input.

We also expect that you, as a student in this course, will honor and respect
your classmates, abiding by the UCSD Principles of Community
(https://ucsd.edu/about/principles.html).  Please understand that others’
backgrounds, perspectives and experiences may be different than your own, and
help us to build an environment where everyone is respected and feels
comfortable.

If you experience any sort of harassment or discrimination, please contact the
instructor as soon as possible.   If you prefer to speak with someone outside
of the course, please contact the Office of Prevention of Harassment and
Discrimination: https://ophd.ucsd.edu/.
