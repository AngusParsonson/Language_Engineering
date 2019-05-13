> import Data.List

Deep embedding -----------------------------------------------------------------

> data Graph = Empty
>            | Vertex  Int
>            | Overlay Graph Graph
>            | Connect Graph Graph
>   deriving Show

> vertices :: Graph -> [Int]
> vertices  Empty          = []
> vertices (Vertex x)      = [x]
> vertices (Overlay gl gr) = nub (vertices gl ++ vertices gr)
> vertices (Connect gl gr) = nub (vertices gl ++ vertices gr)

> edges :: Graph -> Int
> edges  Empty          = 0
> edges (Vertex x)      = 0
> edges (Overlay gl gr) = edges gl + edges gr
> edges (Connect gl gr) = (length(vertices gl) * length(vertices gr)) + edges gl + edges gr

> roots :: Graph -> [Int]
> roots Empty = []
> roots (Vertex x) = [x]
> roots (Overlay gl gr) = roots gl ++ roots gr
> roots (Connect gl gr) = (roots gl \\ vertices gr)

Shallow embedding --------------------------------------------------------------

> type GraphShal = ([Int], Int)

> empty :: GraphShal
> empty = ([], 0)

> vertex :: Int -> GraphShal
> vertex n = ([n], 0)

> overlay :: GraphShal -> GraphShal -> GraphShal
> overlay gl gr = (fst(gl) ++ fst(gr), snd(gl) + snd(gr))

> connect :: GraphShal -> GraphShal -> GraphShal
> connect gl gr = (fst(gl) ++ fst(gr), (length (fst gl)) * (length (fst gr)) + snd gl + snd gr)

Classy embedding ---------------------------------------------------------------

> class Graphy graph where
>   empty'   :: graph
>   vertex'  :: Int -> graph
>   overlay' :: graph -> graph -> graph
>   connect' :: graph -> graph -> graph

> newtype Vertices = Vertices [Int]
> instance Graphy Vertices where
>   empty'                               = Vertices []
>   vertex'   n                          = Vertices [n]
>   overlay' (Vertices gl) (Vertices gr) = Vertices (gl ++ gr)
>   connect' (Vertices gl) (Vertices gr) = Vertices (gl ++ gr)

> newtype Edges = Edges ([Int], Int)
> instance Graphy Edges where
>   empty'                         = Edges ([], 0)
>   vertex'   n                    = Edges ([n], 0)
>   overlay' (Edges gl) (Edges gr) = Edges (fst(gl) ++ fst(gr), snd(gl) + snd(gr))
>   connect' (Edges gl) (Edges gr) = Edges (fst(gl) ++ fst(gr), (length(fst(gl)) * length(fst(gr))) + (snd(gl) + snd(gr)))
