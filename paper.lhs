\documentclass{siamltex}

\usepackage{amsmath}
\usepackage{tikz}
\usetikzlibrary{shapes,snakes}

\usepackage{fancyvrb}
\usepackage{verbatim}
\DefineVerbatimEnvironment{code}{Verbatim}{fontsize=\small}
\DefineVerbatimEnvironment{example}{Verbatim}{fontsize=\small}
\newcommand{\ignore}[1]{}

\newcommand{\newword}[1]{\textit{#1}}

\title{Winning the Game of Ghost with Prefix Trees, Game Trees, and Small Winning Trees}
\author{Peter Boothe\thanks{Mathematics and Computer Science Department,
Manhattan College, 3840 Corlear Ave, Bronx, New York, 10463
({\tt peter.boothe@manhattan.edu}).}}
\begin{document}

\maketitle

\begin{abstract}
We discuss the game of Ghost and then show how to completely solve it on a
given dictionary.  In doing so, we expose an interesting symmetry between the
prefix tree data structure and the game tree of the game.  We end by showing
how to produce a minimal tree so that the player who can win has to memorize as
little as possible.  Along the way, we provide an introduction to the
programming language Haskell, which attempts to bring the idea of a ``function
inside of a program'' in line with the mathematical conception of a function as
a 1-1 mapping from a domain to a range.
\end{abstract}

\begin{keywords} 
game theory, prefix tree, data structure, game tree, word game
\end{keywords}

\begin{AMS}
68P05, 68N18, 91A46, 97R80
\end{AMS}

\pagestyle{myheadings}
\thispagestyle{plain}
\markboth{P. BOOTHE}{WINNING THE GAME OF GHOST USING TREES}

\section{Introduction} The game of Ghost is a dictionary game that tests the
vocabulary (and game theory) of two players: Alice and Bob.  The object of the
game is to not be the person who is forced to complete a word.  The game is
played as follows:  The first player (Alice) says a letter of the alphabet that
is allowed to begin a word.  If that letter is a complete word in and of
itself, Alice loses\footnote{Obvious corollary: do not start with ``a'' or
``i'' if you are the first player.}.  The second player (Bob) says another
letter.  If the composition of the first and second letters completes a word,
Bob loses.  Bob also loses if no word in the dictionary begins with the string
built up over the course of gameplay.

Play proceeds back and forth until either the built-up sequence of characters
completes a dictionary word, or the built-up string is not a prefix of a
dictionary word.  In all cases, the last player to play is said to have lost.
The mis\`ere\footnote{Swap the winning and losing conditions on a game to
generate the mis\`ere version.  In Ghost, this means that the winner is the
player who makes a gibberish prefix or manages to complete a word.  Chess,
checkers, and all the classic games of game theory have been analyzed both
ways, and many questions about mis\`ere games remain open to this day.  In any
dictionary with single-letter words, the mis\`ere version of Ghost is trivial.
Because Scrabble prohibits single-letter words, there are no such words in the
Scrabble dictionary.  Modifying the given code to solve Mis\`ere Ghost is a fun
exercise left to the interested reader.} version of the game is an
immediate win for Alice in any language with single-letter words, but may be
more interesting if single-character words are eliminated from the dictionary.
Two example game transcripts may be seen in Figure~\ref{fig:transcript}.

\begin{figure}
\begin{tabular}{p{2.35in} | p{2.35in}}
\hfill Game 1 \hfill ~ & \hfill Game 2 \hfill ~ \\
\hline
Alice: I say `G'

Bob: I say `H' 

Alice: I say `O'

Bob: I say `S'

Alice: I say `T'

Bob: You just completed the word `GHOST', so I win and you lose!  
&
Alice: I say `G'

Bob: I say `H' 

Alice: I say `O'

Bob: I say `X'

Alice: No word in the dictionary begins with `GHOX', and gibberish is forbidden, so I win and you lose!
\\
\hline
\hfill Bob wins. \hfill ~ & \hfill Alice wins. \hfill ~
\end{tabular}

\caption{Two sample games of Ghost, with two different outcomes.}
\label{fig:transcript}
\end{figure}

In order to resolve disputes, players must agree on a dictionary before play
begins. In the case of the games in Figure~\ref{fig:transcript} this agreement
is a very useful thing, because if Bob claims in game 2 that there is a word
that starts with `GHOX', then the game ceases to be a game becomes an argument.
In the presence of an agreed-upon dictionary, the players can look up the
contested word and have an uncontestable result.

The game of Ghost contains no randomness (no dice or coin flips), and there are
no secrets between the two players (no cards that only one player can see).
Gomes of this nature are called XXXXX, and were first studied in XXXX by YYYYY.
In analyzing games of this type, YYYY constructed what we call a game tree.

A game tree specifies the complete state of the game, how that state was
reached, and all possible subgames which might result from a given position.
Every vertex of the tree represents a game state and a single player's turn,
and every edge represents a move that one player is allowed to do in order to
make it another player's turn.  To concretize this, we will look at the game of
Ghost with an extremely simple dictionary.  Let us say that Alice and Bob agree
that they should use a dictionary containing only 4 words: ghost, green, tan,
tree, and tries.  This means that the first player, Alice, can only choose from
two letters if she would like to avoid creating gibberish!  The full tree for
this game may be seen in Figure~\ref{fig:tinytree}.

\begin{figure}
\begin{center}
\begin{tikzpicture}[
        rc/.style={color=white,fill=red!70,circle},
        bc/.style={color=white,fill=blue!70,circle},
        rs/.style={color=white,fill=red!70,star,star points=7},
        bs/.style={color=white,fill=blue!70,star,star points=7},
]
\node (root) [grow=down] {Game Start} child {
[sibling distance=35mm]
node [rc]{} 
    child {
        [sibling distance=15mm]
        node [bc]{}
        child { node [rc]{}
            child { node [bc]{}
                child { node [rc]{}
                    child { node [bs]{}
                        edge from parent node [left] {t}
                    }
                    edge from parent node [left] {s}
                }
                edge from parent node [left] {o}
            }
            edge from parent node [left] {h}
        }
        child { node [rc]{}
            child { node [bc]{}
                child { node [rc]{}
                    child { node [bs]{}
                        edge from parent node [right] {n}
                    }
                    edge from parent node [right] {e}
                }
                edge from parent node [right] {e}
            }
            edge from parent node [right] {r}
        }
        edge from parent node [above] {g}
    }
    child {
        [sibling distance=17mm]
        node [bc]{}
        child { node [rc]{}
            child { node [bs]{}
                edge from parent node [left] {n}
            }
            edge from parent node [left] {a}
        }
        child { 
            [sibling distance=10mm]
            node [rc]{}
            child { node [bc]{} 
                child { node [rs]{}
                    edge from parent node [left] {e}
                }
                edge from parent node [left] {e}
            }
            child { node [bc]{}
                child { node [rs]{}
                    child [grow=right] { node {}
                        edge from parent [draw=none]
                        child [grow=right] {
                            node {Turn 5 (Alice's turn)}
                            edge from parent [draw=none]
                            child [grow=down] {
                                node {Turn 6 (Bob's turn)}
                                edge from parent [draw=none]
                            }
                            child [grow=up] {
                                node {Turn 4 (Bob's turn)}
                                edge from parent [draw=none]
                                child [grow=up] {
                                    node {Turn 3 (Alice's turn)}
                                    edge from parent [draw=none]
                                    child [grow=up] {
                                        node {Turn 2 (Bob's turn)}
                                        edge from parent [draw=none]
                                        child [grow=up] {
                                            node {Turn 1 (Alice's turn)}
                                            edge from parent [draw=none]
                                        }
                                    }
                                }
                            }
                        }
                    }
                    edge from parent node [right] {e}
                }
                edge from parent node [right] {i}
            }
            edge from parent node [right] {r}
        }
        edge from parent node [above] {t}
    }
};
\end{tikzpicture}
\end{center}
\caption{The complete game tree for the game of Ghost played with a dictionary
containing only the words: ``ghost'', ``green'', ``tan'', ``tree'', and
``trie''.  Every star represents a winning end for that level's corresponding
player.  Note that here, if Bob plays cleverly, he can always win, no matter
what Alice does.  This tree could be considered incomplete, as we have omitted
all of the gibberish moves, which are immediate losses for the moving player.}
\label{fig:tinytree}
\end{figure}

When a game is completely described by a game tree, and every leaf of the tree
involves one player or the other winning, then there exists a \newword{winning
strategy} for one (and only one) of the players.  A winning strategy is a set
of moves which, if performed, allows the moving to be guaranteed of a win.
Tic-tac-toe has no winning strategy, because despite its simple game tree, many
of the leaves of the tree (aka ends of the game) involve neither player
winning.  In the example given in Figure~\ref{fig:tinytree}, Bob has a winning
strategy.  Of course, with an aim towards actual formality, we can define this
more rigorously in the form of a logical statement.

Let us define logical variables $\{a_i | 0 \le i \le n\}$, where $n$ is the
maximum number of moves in a game, and move variables $m_i$, where $m_i$ is a
is an element of the set of all legal moves at turn $i$, given previous moves
$\{m_1, \cdots m_{i-1}\}$, with $m_1$ being an element from the set of all legal starting moves.  If $a_i$ is true, then Alice has won on move $i$.
Therefore, to encode the idea of Alice having a winning strategy in the game of
Ghost where the word in the dictionary is of maximum length $n$ (which
corresponds to the maximum number of moves in the game), we say that there
exists a move such that Alice immediately wins, or, no matter what Bob responds
with, Alice can still win.  Formally, for the example in
Figure~\ref{fig:tinytree} with maximum length 5, we say 
\[a_0 \lor (\exists m_1~[ a_1 \lor (\forall m_2~[ a_2 \lor (\exists m_3~[ a_3 \lor (\forall m_4~[ a_4 \lor (\exists m_5~[ a_5])])])])])\]
This complicated expression corresponds to an even-more-complicated english
phrase:
\begin{quote} Either Alice wins the game by not moving at all ($a_0$) or there
exists a first move ($m_1$) that either causes her to immediately win ($a_1$),
or, for all of Bob's possible responses ($m_2$) to that first move, either Alice
immediately wins ($a_2$) or there exists a move ($m_3$) for Alice where either
she immediately wins ($a_3$) or, for all of Bob's possible responses ($m_4$), either Alice immediately wins ($a_4$) or there exists a move ($m_5$) that causes her to win.\end{quote}
This tortured expression presents an immediate argument for the benefits of
mathematical notation.  Unfortunately, our notation still has a little
ambiguity.  In particular, each $a_i$ is a function of both the dictionary, and
all the previous moves $m_1 \cdots m_i$, and the set of legal moves at any
given point is also a function of the dictionary and all the previous moves.
Thus, to be totally explicit, we should take our dictionary, a set of
words $D$, and define two functions: $v$ and $w$.  The function $v$ will take
as input the dictionary $D$ and the list of all prior moves, and it will return
a set of legal moves.  The function $w$ will take as input the dictionary $D$
and all prior moves, and return whether the current player has just won the
game.  Our final logical expression is then:
\begin{align}
w(D) &\lor (\exists m_1 \in v(D)~[ w(D, m_1) \nonumber \\
&\hspace{1em}\lor (\forall m_2 \in v(D, m_1)~[ w(D, m_1, m_2) \nonumber \\
&\hspace{2em}\lor (\exists m_3 \in v(D, m_1, m_2)~[ w(D, m_1, m_2, m_3) \\
&\hspace{3em}\lor (\forall m_4 \in v(D, m_1, m_2, m_3)~[ w(D, m_1, m_2, m_3, m_4) \nonumber \\
&\hspace{4em}\lor (\exists m_5 \in v(D, m_1, m_2, m_3, m_4)~[ w(D, m_1, m_2, m_3, m_4, m_5)])])])])]) \nonumber
\end{align}


Once we have our expression written down completely, we can see both that it is
impractically large and nested to work with, and that all of the terms of the
expression are well-defined within the expression once $D$ is specified --- it
has no \newword{free variables}.  This means that, for a particular dictionary,
the given statement is either true or it is false.  If the statement is true,
then there exists a winning strategy for Alice.  If the statement is not true,
then there does not exist a winning strategy for Alice.  Because someone must
win this game, if Alice can not be guaranteed a win, then Bob can be.  Thus,
determining a winning strategy for a game of no chance and perfect information
comes down to the statisfaction of a logical expression.

Somewhat surprisingly, this duality between expressions and games can encode
any computation at all.  The interested reader is referred to ``Games, Puzzles,
and Computation'' by Hearn and Demaine\cite{gpc}, which explores this duality
in great depth and discusses the  ...

\section{Prefix Trees}

Prefix trees were introduced as ``tries'' by Edward Fredkin in
1960\cite{fredkin} based on the idea of dictionary re{\bf trie}val, but resultant confusion about how to pronounce the name
(both ``tree'' and ``try'' are used) as well as the proliferation of other kinds of tree, have led people to the more modern name of prefix tree.

From a computer science standpoint, building a solver for Ghost can be quite
interesting.  Our solution allows us to combine a prefix tree and a game tree.
In particular, the state of the game can be encoded as a node of the prefix
tree of the dictionary, and the set of legal moves is exactly the set of
children of that prefix tree node.  This allows us, if given a dictionary, to
efficiently solve the game of Ghost.

Our solution will create a prefix tree from a dictionary, translate that prefix
tree into a game tree, and then perform min-max search on the game tree.  Of
particular interest is programmatically discovering a subtree which is small
enough to be ``memorizeable'' so that the player with the guaranteed win will
be able to win without computer help in the future.  This document will be both
an explication of how to do each of these, as well as a Literate Haskell
program which solves the whole problem.

\section{A Functional Trie}
Our first order of business is to read in a dictionary from a file and turn the
word data into a trie.  In the spirit of the rules of Scrabble, we will
eliminate from consideration all proper nouns (a.k.a. capitalized words) and
words with embedded punctuation (contractions and the like).  To be truly in
the spirit of Scrabble, we use as our word list the official Scrabble players
word list, widely available online as {\tt twl06.txt}.  The choice of word list
is, of course, completely arbitrary, but the Scrabble list serves our purposes
well because it represents actual words, instead of the more common lists of
``character sequences which a spell-ckecker should accept'', which includes
many abbreviations, acronyms, airport codes, and other non-words.

To read the data into the Trie, we define the appropriate datatype by
simplifying the definition from the Wikipedia page for Trie and then defining
our insert function appropriately.

\begin{code}
module Main where
import qualified Data.Map as Map

data Trie a =
     Trie { value :: a
          , children :: Map.Map Char (Trie a) }

data Ending = Middle | End

emptyTrie :: Trie Ending
emptyTrie = Trie Middle Map.empty

insert :: String -> Trie Ending -> Trie Ending
insert "" t = Trie End Map.empty
insert (first:word) trie = 
    let 
        kids = children trie
        kid = Map.findWithDefault emptyTrie first kids
    in 
        case value trie of
            Middle -> Trie Middle (Map.insert first (insert word kid) kids)
            End -> Trie End Map.empty
\end{code}

Once we have a trie, our next concern is the translation from trie into game tree.

\section{From Prefix Tree to Game Tree}

To translate the trie into a game tree, we begin by noting that our players
alternate turns.  Therefore, all even moves are Alice's moves, and all the odd
moves are Bob's moves.  We will define a player data type and some
corresponding helper functions, and then use the player's turn as the data for
the trie nodes.

\begin{code}
data Player = Alice | Bob

other :: Player -> Player
other Alice = Bob
other Bob = Alice

same :: Player -> Player -> Bool
same Alice Alice = True
same Bob Bob = True
same Alice Bob = False
same Bob Alice = False
\end{code}

Note that our game tree is almost exactly the same as the trie datatype, and so
storing the player turn at every trie node is quite easy.

\begin{code}
trieToGameTrie :: Player -> Trie Ending -> Trie Player
trieToGameTrie player trie = 
    let 
        kids = children trie
        newkids = Map.map (trieToGameTrie (other player)) kids
    in
        Trie player newkids
\end{code}

Once this conversion has been accomplished, we turn our eyes towards min-max
search.

\section{Finding the Winner}

Because there is no way to tie the game, one player or the other must win.
Given a game of no chance with perfect information, there exists a strategy
tree for one of the players where, no matter what the other player does, the
strategy guarantees a win.  To find this tree, we engage in min-max search
(also called alpha-beta search, which is a nice linguistic reference to the
alphabet in our tree of characters).

In min-max search, we attempt to find a starting letter for Alice, such that
for all responses Bob could give, there exists a letter for Alice, such that
for all responses Bob could give, there exists a letter for Alice, such that
for all responses Bob could give, there exists a letter for Alice, ...  such
that Alice wins the game.  In this search, we alternate logical conditions at
each level.  In a strategy tree that makes Alice the winner it must be true
that there exists a winning move for Alice on every odd level, however it mst
also be true that on every even level, all moves that Bob might make will
eventually result in a win for Alice.

Alternatively, we can imagine our game tree being annotated with data
indicating which player wins for a particular subtree. If it is Alice's turn
and there are no moves, then it must be true that Bob just finished a word, in
which case Alice is the winner.  If it is Bob's turn and there are no moves,
then it must be true that Alice just finished a word, and therefore Bob is the
winner.  If it is Alice's turn, and there exists a tree where Alice can win,
then Alice is the winner, otherwise Bob wins.  If it is Bob's turn, and there
exists a tree where Bob can win, then Bob is the winner, other wise Alice wins.
We build up a labeled trie to encode exactly this intuition.  Each node of the
tree will encode both the player turn, as well as the winning player for that
subtree.

\begin{code}
data GameNode =
    GameNode { winner :: Player
             , turn   :: Player }
                 
alphaBeta :: Trie Player -> Trie GameNode
alphaBeta (Trie player moves) = 
    if Map.null moves 
    then
        Trie (GameNode player player) Map.empty
    else
        let
           submoves = Map.map alphaBeta moves
           iswinner x = same player (winner (value x))
        in
           if (Map.size (Map.filter iswinner submoves)) > 0
           then Trie (GameNode player player) submoves
           else Trie (GameNode (other player) player) submoves
\end{code}

Once we have read in a word list, filtered the words, made a trie, made a trie
with moves, and then used those moves to figure out who wins, all that remains
is to look at the winner stored in the root node of the tree, which we will
save as a string for future printing.

\begin{code}
gameWinner :: Trie GameNode -> Player
gameWinner = winner . value 

showPlayer :: Player -> String
showPlayer Alice = "Alice"
showPlayer Bob = "Bob"
\end{code}

Unfortunately, just printing the winner is not really enough.  Ideally, we
would like to find the smallest subtree that will allow us to win.

\section{Winning With Minimal Memorization}

To do this, we go through all of the letters of the alphabet which are still a
win for the winning player, find the tree which is the smallest, and then print
out that tree.  Assuming Alice is the winner, then when rinting out the tree,
for every Alice node will print the winning move with the smallest tree, but
for Bob we must print all possible moves Bob may make.  We begin by defining a
function which will take as its input a solved trie, and output a minimal trie,
and in order to do that, we must define a size function for tries.

\begin{code}
size :: Trie a -> Integer
size (Trie _ kids) = 
    if Map.null kids
    then 1
    else sum (Map.elems (Map.map size kids))
\end{code}

Now, armed with our size function, we want to only include the smallest subtree
when it is the overall winner's turn.  If all moves are equally good, then we
will only try to require memorization of the smallest subtree in every case.

\begin{code}
minimize :: Player -> Trie GameNode -> Trie GameNode
minimize bigwinner (Trie gamenode kids) =
    if Map.null kids
    then Trie gamenode kids
    else
        let
            minkids = Map.map (minimize bigwinner) kids
            kidlist = (Map.toList minkids)
            iswinningmove (c,t) = same (gameWinner t) bigwinner
            winningmoves = filter iswinningmove kidlist
            smallerpair (c1, t1) (c2, t2) = 
                    if (size t1) < (size t2)
                    then (c1, t1)
                    else (c2, t2)
            bestpair = foldr1 smallerpair winningmoves
        in
            if same bigwinner (turn gamenode)
            then Trie gamenode (Map.fromList [ bestpair ])
            else Trie gamenode minkids
\end{code}

Once the tree has been minimized, all that remains is to print it out, which
we do in alphabetic order.

\begin{code}
printGameTrie :: String -> Trie GameNode -> String
printGameTrie prefix (Trie gn kids) =
        if Map.null kids
        then 
            prefix ++ " (" 
                   ++ showPlayer (turn gn) 
                   ++ " can't go, so " 
                   ++ showPlayer (winner gn) 
                   ++ " wins)\n"
        else 
            let
                kidlist = Map.toAscList kids
                pgt (c,t) = printGameTrie (" " ++ prefix ++ [c]) t
                kidstrings = map pgt kidlist
            in
                prefix ++ "... (" 
                       ++ showPlayer (turn gn) 
                       ++ "'s turn to choose)\n" 
                       ++ (concat kidstrings)
\end{code}

\section{Putting it all together}

\begin{code}
main :: IO ()
main = 
    do 
        contents <- readFile "twl06.txt"
        let validwords = words contents
        let tree = foldr insert emptyTrie validwords
        let gameTree = trieToGameTrie Alice tree 
        let winningTree = alphaBeta gameTree
        let thewinner = gameWinner winningTree
        putStrLn ("The game is a win for " ++ showPlayer thewinner)
        putStrLn (printGameTrie "" (minimize thewinner winningTree))
\end{code}

\bibliographystyle{siam}
\bibliography{bibliography}


\Appendix\section{Output of our program}
We get the output:
%\verbatiminput{ghostout.tex}


\end{document}
