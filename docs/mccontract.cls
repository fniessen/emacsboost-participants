%% mccontract.cls -- LaTeX2e MC contract class
%
% Copyright (C) 2010, 2011, 2012 Francesco Pizzolante

%* TODO

%* Identification

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{mccontract}[2012/02/20 v0.2 Mission Critical IT contract class]

%* --[ Documentation

% See http://en.wikibooks.org/wiki/LaTeX  (= excellent!)

%* Requires

%* Options

%   - `hidemccomment, do not print mccomments
%   - `french', sets the default document language to French (babel)
\newif\if@french

\DeclareOption{french}{%
    \@frenchtrue%
    \ClassInfo{mcreport}{Saw option `french'}}
\DeclareOption*{% code to be executed for every unknown option
    \PassOptionsToClass{\CurrentOption}{article}%
    \PassOptionsToClass{\CurrentOption}{report}%
    \PassOptionsToClass{\CurrentOption}{book}%
}

\ProcessOptions\relax% execute the options


%* Declarations

%* Document Classes (some of the various classes available)

\LoadClass[a4paper,oneside,11pt]{article}

%* Layout (controlling the page layout)

% flexible and complete interface to document dimensions
\RequirePackage[%
    a4paper,%
    dvips,% will set `\pdfpagewidth/height' when run with `pdflatex'
    left=4.5cm,%
    right=2.5cm,%
    top=2.5cm,%
    headsep=2.0cm,%
    bottom=2.2cm,%
    bindingoffset=0cm,%
    footnotesep=24pt plus 5pt minus 5pt,
    % showframe,%  --> use showframe package instead?
    % marginparwidth=65pt,%
    % marginratio=1:1,%
    % tmargin=126pt,%
    % height=550pt,%
    % width=400pt,%
]{geometry}
\RequirePackage[utf8]{inputenc}
\RequirePackage[frenchb, english]{babel}

%* Definitions (define your own commands etc.)

%* Environments (such as enumerate & itemize)

\newcommand{\mcsquare}{\rule[.35ex]{.5ex}{.5ex}}

% control layout of `itemize', `enumerate', `description'
\RequirePackage{enumitem}% (not in medium TeX Live installation)
\setlist{leftmargin=*,%
    labelsep=1.25cm,%
    itemindent=0pt,%
    topsep=5pt,%
    parsep=3pt}
\setitemize[1]{label=\mcsquare}
\setdescription{%
    % style=nextline,
    font=\sffamily\bfseries,%
    labelwidth=0pt,
    labelsep=0.5em,
    leftmargin=1.45cm}


%* Making Paragraphs (paragraph commands)

% paragraph layout with zero `\parindent' and non-zero `\parskip'
\RequirePackage{parskip}

%* Margin Notes (putting remarks in the margin)

%* Page Styles (various styles of page layout)
\let\orig@title\title
\renewcommand{\title}[1]{\orig@title{\textsf{#1}}}

% Define subtitle
% Properly clear the global variables and commands
% (is is more a convention to prevent confusion with
% symbols defined at other scopes)
\global\let\@subtitle\@empty
\global\let\subtitle\relax
% Define the subtitle variable and initialize it
\newcommand{\subtitle}[1]{\def\@subtitle{#1}}
\subtitle{}

\newcommand{\mccompany}[1]{\def\@mccompany{#1}}
\mccompany{INFORMATION SYSTEMS CONSULT, société anonyme inscrite à la Banque
Carrefour des Entreprises sous le n°425.189.602, dont le siège social est sis
à 1000 Bruxelles, Square de Meeûs 40, représentée par Michel VANDEN
BOSSCHE-MARQUETTE, administrateur délégué, ci-après dénommée MCit, d'une
part,}

\newcommand{\clientcompany}[1]{\def\@clientcompany{#1}}
\clientcompany{client company undefined, d'autre part,}

\newcommand{\expose}[1]{\def\@expose{#1}}
\expose{}

\renewcommand{\maketitle}{%
\thispagestyle{plain}%
\begin{center}
    \vskip 5mm
    {\LARGE \textbf{\@title}\par}%
    \ifx \@subtitle \@empty%
    \relax
    \else%
    {\small\@subtitle}
    \fi%
    \vskip 1cm
\end{center}%
\hskip -1cm \textbf{Entre}\par
\@mccompany\par
\hskip -1cm \textbf{et}\par
\@clientcompany\par

\ifx \@expose \@empty%
\relax
\else%
\hskip -1cm\textbf{Il a été exposé ce qui suit.}
\@expose
\fi%

\hskip -1cm\textbf{Il a été convenu ce qui suit.}
}


% extensive control of page headers and footers
\RequirePackage{fancyhdr} % must be loaded after `geometry'
\RequirePackage{lastpage}

\newcommand{\footfont}{\fontsize{7pt}{0pt}\sffamily\selectfont}
\newcommand{\headfont}{\fontsize{8pt}{8pt}\sffamily\selectfont}

% MC page headers definition
\newcommand{\mcheadl}{}
\newcommand{\mcheadr}{}
\newcommand{\mcheadc}{}

% MC page footers definition
\newcommand{\mcfootl}{\footfont\@title}
\newcommand{\mcfootc}{}
\newcommand{\mcfootr}{\footfont page~\thepage~de~\pageref{LastPage}}

\fancypagestyle{empty}{%
   \fancyhf{} % clear all header and footer fields
   \renewcommand{\headrulewidth}{0.0pt}
   \renewcommand{\footrulewidth}{0.0pt}
}

\fancypagestyle{plain}{%
   \fancyhf{} % clear all header and footer fields
   \fancyhfoffset[L]{1cm} % offset to be applied to the header/footer
   \renewcommand{\headrulewidth}{0.0pt}
   \fancyfoot[L]{\mcfootl{}}
   \fancyfoot[C]{\mcfootc{}}
   \fancyfoot[R]{\mcfootr{}}
   \renewcommand{\footrulewidth}{0.4pt}
}

% TODO Put the headers and footers in some gray (#BBC9C9)
\fancypagestyle{headings}{%
   \fancyhf{} % clear all header and footer fields
   \fancyhfoffset[L]{1cm} % offset to be applied to the header/footer
   \fancyhead[L]{\mcheadl{}}
   \fancyhead[R]{\mcheadr{}}
   \fancyhead[C]{\mcheadc{}}
   \renewcommand{\headrulewidth}{0.0pt}
   \fancyfoot[L]{\mcfootl{}}
   \fancyfoot[C]{\mcfootc{}}
   \fancyfoot[R]{\mcfootr{}}
   \renewcommand{\footrulewidth}{0.4pt}
}

\pagestyle{headings}

%* Sectioning (how to section properly)
\newcommand{\mccarticle}[1]{\section{#1}}
\newcommand{\mccparagraph}[1]{\subsection{#1}}

\RequirePackage[compact,calcwidth,explicit]{titlesec}% (not in medium TeX Live installation)

\titlespacing{\section}{-1cm}{12pt}{*0} % left, beforesep, aftersep
\titleformat{\section}%
    {\bfseries\sffamily}%
    {Article~\thesection~-~#1\ignorespaces}%
    {0em}%
    {}%
    [\vspace{-1.5em}\rule{\titlewidth}{1pt}]%

\titlespacing{\subsection}{0pt}{6pt}{0pt}
\titleformat{\subsection}[runin]{}{\llap{\makebox[1cm][l]{\arabic{section}.\arabic{subsection}}}\makesubsectiontitle{#1}}{0pt}{}
\def\makesubsectiontitle#1{\ifblank{#1}{}{#1\nobreak\hfill\break\par}}

%* Spaces & Boxes (all the associated commands)

%* Table of Contents (how to create a table of contents)

%* Typefaces (such as bold, italics etc.)

% package for underlining/emphasizing multiple lines
\RequirePackage[normalem]{ulem}

\RequirePackage[T1]{fontenc}

%% use regular Times at least (even if not Times New Roman)
% \RequirePackage{mathptmx} % supersedes `times' (sets the text font
%                           % to times *and* the math font to times)
\RequirePackage{times}

% character protrusion and font expansion
\RequirePackage{microtype} % reduces amount of hyphens
% \RequirePackage[babel=true,kerning=true]{microtype}

% hyphenation in `texttt' mode (may not work in all situations)
\DeclareRobustCommand\ttfamily%
{\not@math@alphabet\ttfamily\mathtt%
\fontfamily\ttdefault\selectfont\hyphenchar\font=`\-}

%* Cross References (automatic referencing)

% mcdocument
\RequirePackage{mcdocument}

% publishing quality tables
\RequirePackage{array, booktabs}
\RequirePackage{caption}

% Un petit truc pour que les pages qui finissent par un \pagebreak ne
% soient pas ajustées verticalement : redéfinir \pagebreak en préambule :
\let\ancienPagebreak\pagebreak
\renewcommand{\pagebreak}{\vspace*{\fill}\par\ancienPagebreak}


\if@french
    \AtBeginDocument{\selectlanguage{french}}
        \ClassInfo{mccontract}{Using french language}
\else % This is a workaround as when you pass the 'english' option you get the
      % labels in french. This is due to the fact that babel will load the
      % english language first and thus, the french language will become the
      % default one.
    \AtBeginDocument{\selectlanguage{english}}
    \ClassInfo{mccontract}{Using english language}
\fi

%% End of package
\endinput % very last line

% This is for the sake of Emacs.
% Local Variables:
% mode: outline-minor
% ispell-local-dictionary: "en_US"
% time-stamp-format: "%:y/%02m/%02d"
% time-stamp-start: "Provides\\(Class\\|Package\\){[a-zA-Z-]+}\\["
% time-stamp-end: " "
% End:

%% mccontract.cls ends here
