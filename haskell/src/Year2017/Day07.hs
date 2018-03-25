module Year2017.Day07 ( day07 ) where

-- import           Data.List (partition)
-- import           Data.Tree

import           Types


day07 :: Mode -> AB -> String -> IO [String]
day07 mode ab input = return [ result ]
  where
    result =
      case (mode, ab) of
        -- (Test, A) -> drawTree . fmap show . getRootName $ input
        (Test, A) -> show . getRootName $ input
        _ -> error "NOT-IMPLEMENTED"

-- type Tower = Tree (String, Int)

type Name = String
type Weight = Int
type Node' = (Name, Weight, [Name], Maybe Name)

getRootName :: String -> [String]
getRootName = map (\(n,_,_,_) -> n) . filter (\(_,_,_,p) -> p == Nothing) . setParents . parse
  where
    parse :: String -> [Node']
    parse = map tokenize . map words . lines
      where
        tokenize :: [String] -> Node'
        tokenize (n:w:[]) = (n, read w, [], Nothing)
        tokenize (n:w:_:cs) = (n, read w, map (\s -> [ch | ch <- s, ch /= ',']) cs, Nothing)
        tokenize tokens = error $ "INVALID-NODE: " ++ (show tokens)

    setParents :: [Node'] -> [Node']
    setParents ns =
      -- let initial = foldr setParent (False, ns) ns
      --  in snd . until nothingSetThisPass setParents' $ initial
      snd . until nothingSetThisPass setParents' $ (False, ns)
      where
        nothingSetThisPass :: (Bool, [Node']) -> Bool
        nothingSetThisPass = not . fst

        setParents' :: (Bool, [Node']) -> (Bool, [Node'])
        setParents' (set, nodes) =
          -- foldr (\node (set',nodes') -> setParent node) (set, []) nodes
          foldr setParent (set, nodes) nodes

        setParent :: Node' -> (Bool, [Node']) -> (Bool, [Node'])
        setParent (parentName, _, children, _) (set, nodes) = (set || nodes /= nodes', nodes')
          where
            nodes' = map (\(name, w, cs, mParent) -> (name, w, cs, setParent' name mParent)) nodes
            setParent' name mParent = if name `elem` children then Just parentName else mParent
        -- setParent (parentName, weight, children, _) (set, nodes) = (set', childNodes ++ nodes')
            -- set' = set || containedInNodes || length children > 0
            -- childNodes = map (\child -> (child, 0, [], Just parentName)) children
            -- containedInNodes = any (\(n,_,_,_) -> n == parentName) nodes
            -- nodes' =
            --   case containedInNodes of
            --     True -> map (\node@(n,_,cs,p) -> if n == parentName then (parentName, weight, cs, p) else node) nodes
            --     False -> (parentName, weight, [], Nothing):nodes

    -- build :: [Node'] -> Tower
    -- build nodes =
    --   let (leaves, nodes') = partition (\(_,_,p) -> p == Nothing) nodes
    --       leaves' = map (\(n,w,_) -> Node (n,w) []) leaves
    --    in head . snd . until (null . fst) buildTreeLeavesFirst $ (nodes, leaves)
    --
    -- buildTreeLeavesFirst :: ([Node'], [Tower]) -> ([Node'], [Tower])
    -- buildTreeLeavesFirst (nodes, leaves) =
    --   let (ready, nodes') = partition (\(Node (name,_) _) -> ) leaves
    --   --     -- if they belong to [Tower], add to forest
    --   --     -- else leaf Node
    --   --     (leaves, children) = partition (\(Node tag cs) -> any (\(_,_,children) -> name `elem` children )) ready
    --   --  in (nodes', towers')


    -- build :: [Node'] -> Tower
    -- build nodes =
    --   head . subForest . snd . until (null . fst) buildTreeLeavesFirst $ (nodes, Node ("", 0) [])
    --
    -- buildTreeLeavesFirst :: [Node'] -> ([Node'], [Node])
    -- -- buildTreeLeavesFirst (_, tower) = ([], tower)
    -- buildTreeLeavesFirst (nodes, tower) =
    --   let (leaves, nodes') = partition (\(_,_,children) -> null children) nodes
    --    in (nodes', map (\(n,w,_)) leaves)
