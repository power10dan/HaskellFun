
-- Part a 
data Mode = UP | Down
data Pos =  I Int| Var String
data Pars = Pair String Pars | NoPars
data Vals = V Int Vals | NoVals
data Cmd = Pen Mode 
          | MoveTo Pos Pos 
          | Def String [Pars] Cmd 
          | Call String [Vals]
          | Seq Cmd Cmd 

