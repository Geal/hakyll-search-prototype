---
title: Smalltalk
test: "blah"
---

Smalltalk is an object-oriented, dynamically typed, reflective programming language. Smalltalk was created as the language to underpin the "new world" of computing exemplified by "human–computer symbiosis."[1] It was designed and created in part for educational use, more so for constructionist learning, at the Learning Research Group (LRG) of Xerox PARC by Alan Kay, Dan Ingalls, Adele Goldberg, Ted Kaehler, Scott Wallace, and others during the 1970s.

The language was first generally released as Smalltalk-80. Smalltalk-like languages are in continuing active development, and have gathered loyal communities of users around them. ANSI Smalltalk was ratified in 1998 and represents the standard version of Smalltalk.[2]
Contents

    1 History
    2 Influences
    3 Object-oriented programming
    4 Reflection
    5 Syntax
        5.1 Literals
        5.2 Variable declarations
        5.3 Assignment
        5.4 Messages
        5.5 Expressions
        5.6 Code blocks
    6 Control structures
    7 Classes
        7.1 Methods
        7.2 Instantiating classes
    8 Hello World example
    9 Image-based persistence
    10 Level of access
    11 Just-in-time compilation
    12 List of implementations
    13 References
    14 Further reading
    15 External links

History

There are a large number of Smalltalk variants.[3] The unqualified word Smalltalk is often used to indicate the Smalltalk-80 language, the first version to be made publicly available and created in 1980.

Smalltalk was the product of research led by Alan Kay at Xerox Palo Alto Research Center (PARC); Alan Kay designed most of the early Smalltalk versions, which Dan Ingalls implemented. The first version, known as Smalltalk-71, was created by Ingalls in a few mornings on a bet that a programming language based on the idea of message passing inspired by Simula could be implemented in "a page of code."[1] A later variant actually used for research work is now known as Smalltalk-72 and influenced the development of the Actor model. Its syntax and execution model were very different from modern Smalltalk variants.

After significant revisions which froze some aspects of execution semantics to gain performance (by adopting a Simula-like class inheritance model of execution), Smalltalk-76 was created. This system had a development environment featuring most of the now familiar tools, including a class library code browser/editor. Smalltalk-80 added metaclasses, to help maintain the "everything is an object" (except private instance variables) paradigm by associating properties and behavior with individual classes, and even primitives such as integer and boolean values (for example, to support different ways of creating instances).

Smalltalk-80 was the first language variant made available outside of PARC, first as Smalltalk-80 Version 1, given to a small number of firms (Hewlett-Packard, Apple Computer, Tektronix, and DEC) and universities (UC Berkeley) for "peer review" and implementation on their platforms. Later (in 1983) a general availability implementation, known as Smalltalk-80 Version 2, was released as an image (platform-independent file with object definitions) and a virtual machine specification. ANSI Smalltalk has been the standard language reference since 1998.[4]

Two of the currently popular Smalltalk implementation variants are descendants of those original Smalltalk-80 images. Squeak is an open source implementation derived from Smalltalk-80 Version 1 by way of Apple Smalltalk. VisualWorks is derived from Smalltalk-80 version 2 by way of Smalltalk-80 2.5 and ObjectWorks (both products of ParcPlace Systems, a Xerox PARC spin-off company formed to bring Smalltalk to the market). As an interesting link between generations, in 2002 Vassili Bykov implemented Hobbes, a virtual machine running Smalltalk-80 inside VisualWorks.[5] (Dan Ingalls later ported Hobbes to Squeak.)

During the late 1980s to mid-1990s, Smalltalk environments—including support, training and add-ons—were sold by two competing organizations: ParcPlace Systems and Digitalk, both California based. ParcPlace Systems tended to focus on the Unix/Sun Microsystems market, while Digitalk focused on Intel-based PCs running Microsoft Windows or IBM's OS/2. Both firms struggled to take Smalltalk mainstream due to Smalltalk's substantial memory needs, limited run-time performance, and initial lack of supported connectivity to SQL-based relational database servers. While the high price of ParcPlace Smalltalk limited its market penetration to mid-sized and large commercial organizations, the Digitalk products initially tried to reach a wider audience with a lower price. IBM initially supported the Digitalk product, but then entered the market with a Smalltalk product in 1995 called VisualAge/Smalltalk. Easel introduced Enfin at this time on Windows and OS/2. Enfin became far more popular in Europe, as IBM introduced it into IT shops before their development of IBM Smalltalk (later VisualAge). Enfin was later acquired by Cincom Systems, and is now sold under the name ObjectStudio, and is part of the Cincom Smalltalk product suite.

In 1995, ParcPlace and Digitalk merged into ParcPlace-Digitalk and then rebranded in 1997 as ObjectShare, located in Irvine, CA. ObjectShare (NASDAQ: OBJS) was traded publicly until 1999, when it was delisted and dissolved. The merged firm never managed to find an effective response to Java as to market positioning, and by 1997 its owners were looking to sell the business. In 1999, Seagull Software acquired the ObjectShare Java development lab (including the original Smalltalk/V and Visual Smalltalk development team), and still owns VisualSmalltalk, although worldwide distribution rights for the Smalltalk product remained with ObjectShare who then sold them to Cincom.[6] VisualWorks was sold to Cincom and is now part of Cincom Smalltalk. Cincom has backed Smalltalk strongly, releasing multiple new versions of VisualWorks and ObjectStudio each year since 1999.

Cincom, Gemstone and Object Arts, plus other vendors continue to sell Smalltalk environments. IBM has 'end of life'd VisualAge Smalltalk having in the late 1990s decided to back Java and it is, as of 2006, supported by Instantiations, Inc.[7] which has renamed the product VA Smalltalk and released several new versions. The open Squeak implementation has an active community of developers, including many of the original Smalltalk community, and has recently been used to provide the Etoys environment on the OLPC project, a toolkit for developing collaborative applications Croquet Project, and the Open Cobalt virtual world application. GNU Smalltalk is a free software implementation of a derivative of Smalltalk-80 from the GNU project. Last but not least Pharo Smalltalk (a fork of Squeak oriented towards research and use in commercial environments) a new and clean MIT licensed open source Smalltalk that brings fresh ideas and interest into the Smalltalk market and scene.

A significant development, that has spread across all current Smalltalk environments, is the increasing usage of two web frameworks, Seaside and AIDA/Web, to simplify the building of complex web applications. Seaside has seen considerable market interest with Cincom, Gemstone and Instantiations incorporating and extending it.
Influences
[icon]  This section requires expansion. (October 2009)

John Shoch, a member of the LRG at PARC, acknowledged in his 1979 paper Smalltalk's debt to Plato's theory of forms in which an ideal archetype becomes the template from which other objects are derived.[8]

Smalltalk has influenced the wider world of computer programming in four main areas. It inspired the syntax and semantics of other computer programming languages. Secondly, it was a prototype for a model of computation known as message passing. Thirdly, its WIMP GUI inspired the windowing environments of personal computers in the late twentieth and early twenty-first centuries, so much so that the windows of the first Macintosh desktop look almost identical to the MVC windows of Smalltalk-80[citation needed]. Finally, the integrated development environment was the model for a generation of visual programming tools that look like Smalltalk's code browsers and debuggers.

Python and Ruby have reimplemented some Smalltalk ideas in an environment similar to that of AWK or Perl. The Smalltalk "metamodel" also serves as the inspiration for the object model design of Perl 6.

The syntax and runtime behaviour of the Objective-C programming language is strongly influenced by Smalltalk.

There is also a modular Smalltalk-like implementation designed for scripting called S#, or Script.NET. S# uses just-in-time compilation technology and supports an extended Smalltalk-like language written by David Simmons of Smallscript Corp.[9][10]
