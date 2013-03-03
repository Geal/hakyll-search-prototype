---
title: Rosa Rosa Rosam
test: aaa
---

Functional programming
From Wikipedia, the free encyclopedia
  (Redirected from Functional languages)
Jump to: navigation, search
For subroutine-oriented programming, see Procedural programming.
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

In computer science, functional programming is a programming paradigm that treats computation as the evaluation of mathematical functions and avoids state and mutable data. It emphasizes the application of functions, in contrast to the imperative programming style, which emphasizes changes in state.[1] Functional programming has its roots in lambda calculus, a formal system developed in the 1930s to investigate computability, the Entscheidungsproblem, function definition, function application, and recursion. Many functional programming languages can be viewed as elaborations on the lambda calculus.[1]

In practice, the difference between a mathematical function and the notion of a function used in imperative programming is that imperative functions can have side effects that may change the value of program state. Because of this, they lack referential transparency, i.e. the same language expression can result in different values at different times depending on the state of the executing program. Conversely, in functional code, the output value of a function depends only on the arguments that are input to the function, so calling a function f twice with the same value for an argument x will produce the same result f(x) both times. Eliminating side effects can make it much easier to understand and predict the behavior of a program, which is one of the key motivations for the development of functional programming.[1]

Functional programming languages, especially purely functional ones such as the pioneering Hope, have largely been emphasized in academia rather than in commercial software development. However, prominent functional programming languages such as Common Lisp, Scheme,[2][3][4][5] ISLISP, Clojure, Racket,[6] Erlang,[7][8][9] OCaml,[10][11] Haskell,[12][13] Scala[14] and F#[15][16] have been used in industrial and commercial applications by a wide variety of organizations. Functional programming is also supported in some domain-specific programming languages like R (statistics),[17][18] Mathematica (symbolic and numeric math),[19] J, K and Q from Kx Systems (financial analysis), XQuery/XSLT (XML)[20][21] and Opal.[22] Widespread domain-specific declarative languages like SQL and Lex/Yacc use some elements of functional programming, especially in eschewing mutable values.[23]

Programming in a functional style can also be accomplished in languages that aren't specifically designed for functional programming. For example, the imperative Perl programming language has been the subject of a book describing how to apply functional programming concepts.[24] C# 3.0 added constructs to facilitate the functional style.
Contents

    1 History
    2 Concepts
        2.1 First-class and higher-order functions
        2.2 Pure functions
        2.3 Recursion
        2.4 Strict versus non-strict evaluation
        2.5 Type systems
        2.6 Functional programming in non-functional languages
    3 Comparison to imperative programming
        3.1 Simulating state
        3.2 Efficiency issues
        3.3 Coding styles
            3.3.1 Haskell
            3.3.2 Erlang
            3.3.3 Lisp
    4 Use in industry
    5 See also
    6 References
    7 Further reading
    8 External links

History

Lambda calculus provides a theoretical framework for describing functions and their evaluation. Although it is a mathematical abstraction rather than a programming language, it forms the basis of almost all functional programming languages today. An equivalent theoretical formulation, combinatory logic, is commonly perceived as more abstract than lambda calculus and preceded it in invention. It is used in some esoteric languages including Unlambda. Combinatory logic and lambda calculus were both originally developed to achieve a clearer approach to the foundations of mathematics.[25]

An early functional-flavored language was Lisp, developed by John McCarthy while at Massachusetts Institute of Technology (MIT) for the IBM 700/7000 series scientific computers in the late 1950s.[26] Lisp introduced many features now found in functional languages, though Lisp is technically a multi-paradigm language. Scheme and Dylan were later attempts to simplify and improve Lisp.

Information Processing Language (IPL) is sometimes cited as the first computer-based functional programming language.[27] It is an assembly-style language for manipulating lists of symbols. It does have a notion of "generator", which amounts to a function accepting a function as an argument, and, since it is an assembly-level language, code can be used as data, so IPL can be regarded as having higher-order functions. However, it relies heavily on mutating list structure and similar imperative features.

Kenneth E. Iverson developed APL in the early 1960s, described in his 1962 book A Programming Language (ISBN 9780471430148). APL was the primary influence on John Backus's FP. In the early 1990s, Iverson and Roger Hui created J. In the mid 1990s, Arthur Whitney, who had previously worked with Iverson, created K, which is used commercially in financial industries along with its descendant Q.

John Backus presented FP in his 1977 Turing Award lecture "Can Programming Be Liberated From the von Neumann Style? A Functional Style and its Algebra of Programs".[28] He defines functional programs as being built up in a hierarchical way by means of "combining forms" that allow an "algebra of programs"; in modern language, this means that functional programs follow the principle of compositionality. Backus's paper popularized research into functional programming, though it emphasized function-level programming rather than the lambda-calculus style which has come to be associated with functional programming.

In the 1970s, ML was created by Robin Milner at the University of Edinburgh, and David Turner developed initially the language SASL at the University of St. Andrews and later the language Miranda at the University of Kent. ML eventually developed into several dialects, the most common of which are now OCaml and Standard ML. Also in the 1970s, the development of Scheme (a partly functional dialect of Lisp), as described in the influential Lambda Papers and the 1985 textbook Structure and Interpretation of Computer Programs, brought awareness of the power of functional programming to the wider programming-languages community.

In the 1980s, Per Martin-Löf developed intuitionistic type theory (also called constructive type theory), which associated functional programs with constructive proofs of arbitrarily complex mathematical propositions expressed as dependent types. This led to powerful new approaches to interactive theorem proving and has influenced the development of many subsequent functional programming languages.

The Haskell language began with a consensus in 1987 to form an open standard for functional programming research; implementation releases have been ongoing since 1990.
