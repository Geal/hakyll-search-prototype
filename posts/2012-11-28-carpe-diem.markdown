---
test: <truc>
title: Carpe Diem
---
Programming paradigm
From Wikipedia, the free encyclopedia
Jump to: navigation, search
  This article needs additional citations for verification. Please help improve this article by adding citations to reliable sources. Unsourced material may be challenged and removed. (February 2011)
  This article needs attention from an expert in Computer science. Please add a reason or a talk parameter to this template to explain the issue with the article. WikiProject Computer science or the Computer science Portal may be able to help recruit an expert. (January 2009)
Programming paradigms

    Action
    Agent-oriented
    Aspect-oriented
    Automata-based
    Component-based
        Flow-based
        Pipelined
    Concatenative
    Concurrent computing
        Relativistic programming
    Data-driven
    Declarative (contrast: Imperative)
        Constraint
        Dataflow
            Cell-oriented (spreadsheets)
            Reactive
            Intensional
        Functional
        Logic
            Abductive logic
            Answer set
            Constraint logic
            Functional logic
            Inductive logic
    End-user programming
    Event-driven
        Service-oriented
        Time-driven
    Expression-oriented
    Feature-oriented
    Function-level (contrast: Value-level)
    Generic
    Imperative (contrast: Declarative)
        Procedural
    Language-oriented
        Discipline-specific
        Domain-specific
        Grammar-oriented
            Dialecting
        Intentional
    Metaprogramming
        Automatic
        Reflective
            Attribute-oriented
        Homoiconic
        Template
            Policy-based
    Non-structured (contrast: Structured)
        Array
    Nondeterministic
    Parallel computing
        Process-oriented
    Programming in the large and small
    Semantic
    Structured (contrast: Non-structured)
        Modular (contrast: Monolithic)
        Object-oriented
            By separation of concerns:
                Aspect-oriented
                Role-oriented
                Subject-oriented
            Class-based
            Prototype-based
        Recursive
    Value-level (contrast: Function-level)

    v
    t
    e

A programming paradigm is a fundamental style of computer programming. There are four main paradigms: object-oriented, imperative, functional and declarative.[1] Their foundations are distinct models of computation: Turing machine for object-oriented and imperative programming, lambda calculus for functional programming, and first order logic for logic programming.
Contents

    1 Overview
    2 Multi-paradigm programming language
    3 History
    4 See also
    5 References
    6 External links

Overview

A programming model is an abstraction of a computer system. For example, the "von Neumann model" is a model used in traditional sequential computers. For parallel computing, there are many possible models typically reflecting different ways processors can be interconnected. The most common are based on shared memory, distributed memory with message passing, or a hybrid of the two.

A programming language can support multiple paradigms. For example, programs written in C++ or Object Pascal can be purely procedural, or purely object-oriented, or contain elements of both paradigms. Software designers and programmers decide how to use those paradigm elements.

In object-oriented programming, programmers can think of a program as a collection of interacting objects, while in functional programming a program can be thought of as a sequence of stateless function evaluations. When programming computers or systems with many processors, process-oriented programming allows programmers to think about applications as sets of concurrent processes acting upon logically shared data structures.

Just as different groups in software engineering advocate different methodologies, different programming languages advocate different programming paradigms. Some languages are designed to support one particular paradigm (Smalltalk supports object-oriented programming, Haskell supports functional programming), while other programming languages support multiple paradigms (such as Object Pascal, C++, Java, C#, Scala, Visual Basic, Common Lisp, Scheme, Perl, Python, Ruby, Oz and F#).

Many programming paradigms are as well known for what techniques they forbid as for what they enable. For instance, pure functional programming disallows the use of side-effects, while structured programming disallows the use of the goto statement. Partly for this reason, new paradigms are often regarded as doctrinaire or overly rigid by those accustomed to earlier styles.[2] Avoiding certain techniques can make it easier to prove theorems about a program's correctness—or simply to understand its behavior.
Multi-paradigm programming language
See also: List of multi-paradigm programming languages

A multi-paradigm programming language is a programming language that supports more than one programming paradigm[citation needed]. As Leda designer Timothy Budd puts it: "The idea of a multiparadigm language is to provide a framework in which programmers can work in a variety of styles, freely intermixing constructs from different paradigms." The design goal of such languages is to allow programmers to use the best tool for a job, admitting that no one paradigm solves all problems in the easiest or most efficient way.

One example is C#, which includes imperative and object-oriented paradigms as well as some support for functional programming through type inference, anonymous functions and Language Integrated Query. Some other ones are F# and Scala, which provides similar functionality to C# but also includes full support for functional programming (including currying, pattern matching, algebraic data types, lazy evaluation, tail recursion, immutability, etc.). Perhaps the most extreme example is Oz, which has subsets that are logic (Oz descends from logic programming), a functional, an object-oriented, a dataflow concurrent, and other language paradigms. Oz was designed over a ten-year period to combine in a harmonious way concepts that are traditionally associated with different programming paradigms. Lisp, while often taught as a functional language, is known for its malleability and thus its ability to engulf many paradigms.
History

The lowest level programming paradigms are machine code, which directly represents the instructions (the contents of program memory) as a sequence of numbers, and assembly language where the machine instructions are represented by mnemonics and memory addresses can be given symbolic labels. These are sometimes called first- and second-generation languages. In the 1960s assembly languages were developed to support library COPY and quite sophisticated conditional macro generation and pre-processing capabilities, CALL to (subroutines), external variables and common sections (globals), enabling significant code re-use and isolation from hardware specifics via use of logical operators such as READ/WRITE/GET/PUT. Assembly was, and still is, used for time critical systems and frequently in embedded systems as it gives the most direct control of what the machine actually does.

The next advance was the development of procedural languages. These third-generation languages (the first described as high-level languages) use vocabulary related to the problem being solved. For example,

    C - developed c. 1970 at Bell Labs
    COBOL (Common Business Oriented Language) - uses terms like file, move and copy.
    FORTRAN (FORmula TRANslation) - using mathematical language terminology, it was developed mainly for scientific and engineering problems.
    ALGOL (ALGOrithmic Language) - focused on being an appropriate language to define algorithms, while using mathematical language terminology and targeting scientific and engineering problems just like FORTRAN.
    PL/I (Programming Language One) - a hybrid commercial/scientific general purpose language supporting pointers.
    BASIC (Beginners All purpose Symbolic Instruction Code) - was developed to enable more people to write programs.

All these languages follow the procedural paradigm. That is, they describe, step by step, exactly the procedure that should, according to the particular programmer at least, be followed to solve a specific problem. The efficacy and efficiency of any such solution are both therefore entirely subjective and highly dependent on that programmer's experience, inventiveness and ability.

Later, object-oriented languages (like Simula, Smalltalk, C++, Eiffel and Java) were created. In these languages, data, and methods of manipulating the data, are kept as a single unit called an object. The only way that a user can access the data is via the object's 'methods' (subroutines). Because of this, the internal workings of an object may be changed without affecting any code that uses the object. There is still some controversy by notable programmers such as Alexander Stepanov, Richard Stallman[3] and others, concerning the efficacy of the OOP paradigm versus the procedural paradigm. The necessity of every object to have associative methods leads some skeptics to associate OOP with software bloat. Polymorphism was developed as one attempt to resolve this dilemma.

Since object-oriented programming is considered a paradigm, not a language, it is possible to create even an object-oriented assembler language. High Level Assembly (HLA) is an example of this that fully supports advanced data types and object-oriented assembly language programming - despite its early origins. Thus, differing programming paradigms can be thought of as more like 'motivational memes' of their advocates - rather than necessarily representing progress from one level to the next. Precise comparisons of the efficacy of competing paradigms are frequently made more difficult because of new and differing terminology applied to similar (but not identical) entities and processes together with numerous implementation distinctions across languages.

Within imperative programming, which is based on procedural languages, an alternative to the computer-centered hierarchy of structured programming is literate programming, which structures programs instead as a human-centered web, as in a hypertext essay – documentation is integral to the program, and the program is structured following the logic of prose exposition, rather than compiler convenience.

Independent of the imperative branch, declarative programming paradigms were developed. In these languages the computer is told what the problem is, not how to solve the problem - the program is structured as a collection of properties to find in the expected result, not as a procedure to follow. Given a database or a set of rules, the computer tries to find a solution matching all the desired properties. The archetypical example of a declarative language is the fourth generation language SQL, as well as the family of functional languages and logic programming.


