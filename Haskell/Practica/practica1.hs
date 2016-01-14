--------------------------------------
--
-- Data definitions (Part 1)
--
--------------------------------------

data Tree a = Node a [Tree a] | Leaf a
data XMLNode = Element String | Attribute String String | Text String deriving (Show)
type XMLTree = Tree XMLNode
-- we will assume assume:
-- an XMLTree cannot have both Text and Element children nodes;
-- also, it cannot have more than 1 Text child node
-- Attribute and Text nodes are always a Leaf
-- Element nodes are always a Node



--------------------------------------
--
-- Show implementation (Part 1)
--
--------------------------------------

-- is the tree an Attribute?
isAttribute :: XMLTree -> Bool 
isAttribute (Leaf (Attribute _ _)) = True
isAttribute _ = False

-- is the tree Text?
isText :: XMLTree -> Bool 
isText (Leaf (Text _)) = True
isText _ = False

-- is this the kind of list that's used for a tag with Text?
isTextContent :: [XMLTree] -> Bool
isTextContent [] = True
isTextContent l@(x:_) = (length l)==1 && (isText x)

-- given predicate P and a list L, return a pair (A,B), 
-- where A is a list that contains those elements from L that satisfy P
-- and B holds the ones that don't
partition :: (a -> Bool) -> [a] -> ([a], [a]) 
partition _ [] = ([],[])
partition p (x:xs)
  | p x = (x:a,b)
  | otherwise = (a,x:b)
  where (a,b) = partition p xs

-- given the number of 'tabs' T and a string S
-- returns S prefixed with as many 'tabs' as T
-- a 'tab' is two empty spaces, really
indent :: Int -> String -> String
indent 0 s = s
indent n s = ' ':' ':(indent (n-1) s)

-- does the heavy lifting for the show function (indenting stuff or otherwise)
showIndent :: Int -> XMLTree -> String
showIndent _ (Leaf (Attribute name value)) = " " ++ name ++ "=" ++ (show value)
showIndent _ (Leaf (Text text)) = text
showIndent offset (Node (Element tag) children) = opener ++ tagContent ++ closer
  where (attrs,content) = partition isAttribute children
        tagAttrs = concatMap (showIndent (offset+1)) attrs
        tagContent = concatMap (showIndent (offset+1)) content
        -- Text nodes require no line break or indentation for closing tag, so we have to handle that
        textContent = isTextContent content
        contentSeparator = if textContent then "" else "\n"
        rawCloser = "</" ++ tag ++ ">\n"
        -- now we can properly build the tag
        opener = indent offset "<" ++ tag ++ tagAttrs ++ ">" ++ contentSeparator
        closer = if textContent then rawCloser else indent offset rawCloser

-- show definition
instance Show XMLTree where show tree = showIndent 0 tree



--------------------------------------
--
-- Read implementation (Part 1)
--
--------------------------------------

-- in order to be readable text representations of an XML doc:
-- attributes must be written without spaces anywhere between name and value, ie: any="1981 what a year!"
readXMLTree :: String -> XMLTree
readXMLTree s = fst.readXMLTree' $ trim s

-- returns a pair (XMLTree,String) -> the tree and remaining string
readXMLTree' :: String -> (XMLTree,String)
readXMLTree' ('<':xs) = ((Node (Element tag) (attrChildren ++ otherChildren)),readTagClose tag nxs)
  where (rawTag,rtxs) = readRawTag xs
        (tag,txs) = readTagName rawTag
        attrChildren = readAttributes $ trim txs
        (otherChildren,oxs) = handleSubtree rtxs
        ('<':'/':nxs) = trim oxs

-- read contents of an element
handleSubtree :: String -> ([XMLTree],String)
handleSubtree s
  | hasText ts = ([(Leaf (Text txt))],txs)
  | otherwise = readElementChildren ts
  where ts = trim s
        (txt,txs) = readText s

-- yank everything up til tag closer from string
readText :: String -> (String,String)
readText s@('<':xs) = ("",s)
readText (x:xs) = (x:txt,txs)
  where (txt,txs) = readText xs

-- determine if tag is just empty
hasNoChildren :: String -> Bool
hasNoChildren (x1:x2:_) = x1=='<' && x2=='/'

-- is this for Text? It is if it's not a subtag
hasText :: String -> Bool
hasText (x:_) = x/='<'

-- read element children
readElementChildren :: String -> ([XMLTree],String)
readElementChildren s
  | hasNoChildren s = ([],s)
  | otherwise = (element:siblings,sxs)
  where (element,xs) = readXMLTree' s
        (siblings,sxs) = readElementChildren $ trim xs

-- everything inside the tag opener (tag name and attributes)
readRawTag :: String -> (String,String)
readRawTag ('>':xs) = ("",xs)
readRawTag (x:xs) = (x:tag,txs)
  where (tag,txs) = readRawTag xs

-- pull tag name from tag opener
readTagName :: String -> (String,String)
readTagName "" = ("","")
readTagName (' ':xs) = ("",xs)
readTagName (x:xs) = (x:tag,txs)
  where (tag,txs) = readTagName xs

-- pull attribute name
readAttrName :: String -> (String,String)
readAttrName ('=':xs) = ("",xs)
readAttrName (x:xs) = (x:attrName,anxs)
  where (attrName,anxs) = readAttrName xs

-- attribute value
readAttrValue :: String -> (String,String)
readAttrValue ('\"':xs) = ("",xs)
readAttrValue (x:xs) = (x:attrValue,avxs)
  where (attrValue,avxs) = readAttrValue xs

-- pull attribute name-value pair from tag opener
readAttribute :: String -> ((String,String),String)
readAttribute s = ((attrName,attrValue),avxs)
  where (attrName,('\"':anxs)) = readAttrName s
        (attrValue,avxs) = readAttrValue anxs

-- pull all attributes from tag opener into Tree list
readAttributes :: String -> [XMLTree]
readAttributes "" = []
readAttributes s = (Leaf (Attribute attrName attrValue)):(readAttributes $ trim xs)
  where ((attrName,attrValue),xs) = readAttribute s

-- strip tag closer from string
readTagClose :: String -> String -> String
readTagClose "" ('>':xs) = xs
readTagClose (tx:txs) s@(sx:sxs)
  | tx==sx = readTagClose txs sxs
  | otherwise = s

-- remove leading whitespace (just spaces and new lines and tabs)
trim :: String -> String
trim "" = ""
trim (' ':xs) = trim xs
trim ('\n':xs) = trim xs
trim ('\t':xs) = trim xs
trim s = s


 
--------------------------------------
--
-- String to XMLTree (Part 1)
--
--------------------------------------

-- Element from String
readElementNode :: String -> XMLNode
readElementNode s = Element s 

-- Attribute from String
-- String format must be: attrName=attrValue
-- Needs to be fed a correctly formatted String, or it'll produce "Irrefutable pattern failed" error
readAtributeNode :: String -> XMLNode
readAtributeNode s = Attribute attrName attrValue
  where (attrName,'=':attrValue) = span (/='=') s

-- Text from String
readTextNode :: String -> XMLNode
readTextNode s = Text s



--------------------------------------
--
-- Data definitions (Part 2)
--
--------------------------------------

-- query node
class QNode a where
  isKind :: a -> a -> Bool
  geq :: a -> a -> Bool
  leq :: a -> a -> Bool


instance QNode XMLNode where
  -- is kind?
  isKind (Attribute kA vA) (Attribute kB vB) = (kA==kB&&(vA==vB||vB==""))||(kB==""&&vB=="")
  isKind (Element tA) (Element tB) = tA==tB||tB==""
  isKind (Text tA) (Text tB) = tA==tB||tB==""
  isKind _ _ = False
  -- greater or equal?
  geq (Attribute kA vA) (Attribute kB vB) = kA==kB&&((read vA)::Int)>=((read vB)::Int)
  geq _ _ = False
  -- les than or equal?
  leq (Attribute kA vA) (Attribute kB vB) = kA==kB&&((read vA)::Int)<=((read vB)::Int)
  leq _ _ = False


data Condition n = IsKind n | Geq n | Leq n | CTrue | CFalse 
                 | CNot (Condition n) | CAnd (Condition n) (Condition n) | COr (Condition n) (Condition n) 


evaluate :: (QNode a) => a -> Condition a -> Bool
evaluate x (IsKind y) = isKind x y
evaluate x (Geq y) = geq x y
evaluate x (Leq y) = leq x y
evaluate _ (CTrue) = True
evaluate _ (CFalse) = False
evaluate x (CNot c) = not $ evaluate x c
evaluate x (CAnd a b) = (evaluate x a)&&(evaluate x b) 
evaluate x (COr a b) = (evaluate x a)||(evaluate x b) 


data Range = PGe Int | PLe Int | Peq Int
data QTree n = ThisTree                          -- Just the root
             | AnyTree                           -- All trees
             | Selection Range (QTree n)         -- 0-based index!!!
             | RNode (Condition n) [(QTree n)]   -- Either root or query children
             | StarTree (Condition n) (QTree n)  -- Query root and if Condition, query children
             | PlusTree (Condition n) (QTree n)  -- Query root and StarTree children if Condition
             | Union [(QTree n)]                 -- Join results from queries to root



--------------------------------------
--
-- Query methods (Part 2)
--
--------------------------------------

-- xQueryTree -> Return a list of XMLTrees
xQueryTree :: QNode a => QTree a -> Tree a -> [Tree a]

xQueryTree ThisTree t = [t]

xQueryTree AnyTree t@(Node _ l) = t:(concatMap (xQueryTree AnyTree) l)
xQueryTree AnyTree t@(Leaf _) = [t]

xQueryTree (Selection (PGe x) q) t = drop x $ xQueryTree q t
xQueryTree (Selection (PLe x) q) t = take (x+1) $ xQueryTree q t
xQueryTree (Selection (Peq x) q) t = [(xQueryTree q t) !! x]

xQueryTree (RNode c []) t@(Node x _) = if evaluate x c then [t] else []
xQueryTree (RNode c []) t@(Leaf x) = if evaluate x c then [t] else []
xQueryTree (RNode c a) t@(Node x l) = if evaluate x c then xQueryTreeZip a l else []
xQueryTree (RNode _ _) t@(Leaf _) = []

xQueryTree s@(StarTree c q) t@(Node x l)
  | evaluate x c = a++(concatMap (xQueryTree s) l)
  | otherwise = a
  where a = xQueryTree q t
xQueryTree (StarTree _ q) t@(Leaf _) = xQueryTree q t

xQueryTree (PlusTree c q) t@(Node x l)
  | evaluate x c = (xQueryTree q t)++(concatMap (xQueryTree (StarTree c q)) l)
  | otherwise = []
xQueryTree (PlusTree c q) t@(Leaf x) = if evaluate x c then xQueryTree q t else []

xQueryTree (Union l) t = concatMap (`xQueryTree` t) l

xQueryTreeZip :: QNode a => [QTree a] -> [Tree a] -> [Tree a]
xQueryTreeZip _ [] = []
xQueryTreeZip [] (tx:txs) = (xQueryTree ThisTree tx)++(xQueryTreeZip [] txs)
xQueryTreeZip (qx:qxs) (tx:txs) = (xQueryTree qx tx)++(xQueryTreeZip qxs txs)


-- xQueryTree -> Return a list of XMLNodes
xQueryNode :: QNode a => QTree a -> Tree a -> [a]

xQueryNode ThisTree (Node x _) = [x]
xQueryNode ThisTree (Leaf x) = [x]

xQueryNode AnyTree (Node x l) = x:(concatMap (xQueryNode AnyTree) l)
xQueryNode AnyTree (Leaf x) = [x]

xQueryNode (Selection (PGe x) q) t = drop x $ xQueryNode q t
xQueryNode (Selection (PLe x) q) t = take (x+1) $ xQueryNode q t
xQueryNode (Selection (Peq x) q) t = [(xQueryNode q t) !! x]

xQueryNode (RNode c []) (Node x _) = if evaluate x c then [x] else []
xQueryNode (RNode c []) (Leaf x) = if evaluate x c then [x] else []
xQueryNode (RNode c a) (Node x l) = if evaluate x c then xQueryNodeZip a l else []
xQueryNode (RNode _ _) (Leaf _) = []

xQueryNode s@(StarTree c q) t@(Node x l)
  | evaluate x c = a++(concatMap (xQueryNode s) l)
  | otherwise = a
  where a = xQueryNode q t
xQueryNode (StarTree _ q) t@(Leaf _) = xQueryNode q t

xQueryNode (PlusTree c q) t@(Node x l)
  | evaluate x c = (xQueryNode q t)++(concatMap (xQueryNode (StarTree c q)) l)
  | otherwise = []
xQueryNode (PlusTree c q) t@(Leaf x) = if evaluate x c then xQueryNode q t else []

xQueryNode (Union l) t = concatMap (`xQueryNode` t) l

xQueryNodeZip :: QNode a => [QTree a] -> [Tree a] -> [a]
xQueryNodeZip _ [] = []
xQueryNodeZip [] (tx:txs) = (xQueryNode ThisTree tx)++(xQueryNodeZip [] txs)
xQueryNodeZip (qx:qxs) (tx:txs) = (xQueryNode qx tx)++(xQueryNodeZip qxs txs)


-- xRelative
xRelative :: QNode a => QTree a -> QTree a -> Tree a -> [Tree a] 
xRelative q1 q2 t = xRelativeFilter q2 $ xQueryTree q1 t

xRelativeFilter :: QNode a => QTree a -> [Tree a] -> [Tree a] 
xRelativeFilter _ [] = []
xRelativeFilter q (tx:txs)
  | xRelativeFilterFunc q m = tx:r
  | otherwise = r
  where m = xQueryTree AnyTree tx
        r = xRelativeFilter q txs
        
xRelativeFilterFunc :: QNode a => QTree a -> [Tree a] -> Bool
xRelativeFilterFunc _ [] = False
xRelativeFilterFunc q (x:xs)
  | (length (xQueryNode q x))>0 = True
  | otherwise = xRelativeFilterFunc q xs



--------------------------------------
--
-- Test trees
--
--------------------------------------

-- a5 = readXMLTree "<hola any=\"1981\" asd=\"asd\">\n<eo>Ja je  </eo>\n</hola>"
-- a1 = Leaf (Attribute "any" "1981")
-- a2 = Leaf (Text "Howdy")
-- a3 = Node (Element "Hola") [a2,a1]
-- a4 = Node (Element "Uoooo") [a3,a5]
-- a6 = readXMLTree "<a><e></e></a>"
a7 = readXMLTree "<llibres>\n  <llibre any=\"2004\" edicio=\"1\">\n    <titol>Razonando con Haskell</titol>\n    <autor>Blas C. Ruiz</autor>\n    <autor>Francisco Gutierrez</autor>\n    <autor>Pablo Guerrero</autor>\n    <autor>Jose E. Gallardo</autor>\n  </llibre>\n  <llibre edicio=\"2\" any=\"1999\">\n    <titol>HASKELL: The Craft of Functional Programming</titol>\n    <editor>A. D. McGettrick</editor>\n    <autor>Simon Thompson</autor>\n  </llibre>\n  <llibre edicio=\"1\" any=\"2000\">\n    <titol>Programming language pragmatics</titol>\n    <autor>Michael L. Scott</autor>\n  </llibre>\n</llibres>\n"

storageXmlString = "<storage>\n\
\  <disk>\n\
\    <size>500Gb</size>\n\
\    <name>main</name>\n\
\    <folder user=\"xwr\" group=\"r\" other=\"\">\n\
\      <name>home</name>\n\
\      <folder user=\"xwr\" group=\"r\" other=\"\">\n\
\        <name>albert</name>\n\
\       <file user=\"xwr\" group=\"r\" other=\"r\">\n\
\         <name>enunciat.pdf</name>\n\
\         <size>113605</size>\n\
\         <owner>albert</owner>\n\
\       </file>\n\
\       <folder user=\"xwr\" group=\"r\" other=\"\">\n\
\         <name>Haskell</name>\n\
\         <file user=\"xwr\" group=\"r\" other=\"\">\n\
\           <name>practica.hs</name>\n\
\           <size>3330</size>\n\
\           <owner>albert</owner>\n\
\         </file>\n\
\          <file user=\"xwr\" group=\"r\" other=\"\">\n\
\           <name>jp.txt</name>\n\
\           <size>1580</size>\n\
\           <owner>albert</owner>\n\
\         </file>\n\
\       </folder>\n\
\       <file user=\"xwr\" group=\"r\" other=\"r\">\n\
\         <name>enunciat.tex</name>\n\
\         <size>12103</size>\n\
\         <owner>albert</owner>\n\
\       </file>\n\
\      </folder>\n\
\      <folder user=\"xwr\" group=\"r\" other=\"\">\n\
\        <name>daniel</name>\n\
\      </folder>\n\
\      <folder user=\"xwr\" group=\"r\" other=\"\">\n\
\        <name>jordi</name>\n\
\      </folder>\n\
\    </folder>\n\
\    <folder user=\"xwr\" group=\"r\" other=\"\">\n\
\      <name>bin</name>\n\
\      <file user=\"xwr\" group=\"r\" other=\"\">\n\
\        <name>ghc</name>\n\
\        <size>113605</size>\n\
\        <owner>root</owner>\n\
\      </file>\n\
\    </folder>\n\
\  </disk>\n\
\</storage>"

{-
<storage>
  <disk>
    <size>500Gb</size>
    <name>main</name>
    <folder user="xwr" group="r" other="">
      <name>home</name>
      <folder user="xwr" group="r" other="">
        <name>albert</name>
       <file user="xwr" group="r" other="r">
         <name>enunciat.pdf</name>
         <size>113605</size>
         <owner>albert</owner>
       </file>
       <folder user="xwr" group="r" other="">
         <name>Haskell</name>
         <file user="xwr" group="r" other="">
           <name>practica.hs</name>
           <size>3330</size>
           <owner>albert</owner>
         </file>
         <file user="xwr" group="r" other="">
           <name>jp.txt</name>
           <size>1580</size>
           <owner>albert</owner>
         </file>
       </folder>
       <file user="xwr" group="r" other="r">
         <name>enunciat.tex</name>
         <size>12103</size>
         <owner>albert</owner>
       </file>
      </folder>
      <folder user="xwr" group="r" other="">
        <name>daniel</name>
      </folder>
      <folder user="xwr" group="r" other="">
        <name>jordi</name>
      </folder>
    </folder>
    <folder user="xwr" group="r" other="">
      <name>bin</name>
      <file user="xwr" group="r" other="">
        <name>ghc</name>
        <size>113605</size>
        <owner>root</owner>
      </file>
    </folder>
  </disk>
</storage>
-}

storageXmlTree = readXMLTree storageXmlString
rnFalse = RNode CFalse []
eName = [rnFalse,rnFalse,rnFalse,ThisTree,rnFalse,rnFalse]
isFile = RNode (IsKind $ readElementNode "file") eName
plusIsFolder = PlusTree (IsKind $ readElementNode "folder") isFile
starNoFolder x = StarTree (CNot $ IsKind $ readElementNode "folder") x
plusNoFolder x = PlusTree (CNot $ IsKind $ readElementNode "folder") x
selectFrom2 = Selection (PGe 1) $ starNoFolder plusIsFolder
starThisTree = starNoFolder ThisTree
isStorage = RNode (IsKind $ readElementNode "storage") eName




--xRelative (StarTree CTrue (RNode (IsKind (readElementNode "llibre")) [])) (StarTree CTrue (RNode (CAnd (Geq (readAtributeNode "any=1999")) (Leq (readAtributeNode "any=2001"))) [])) llibresXml