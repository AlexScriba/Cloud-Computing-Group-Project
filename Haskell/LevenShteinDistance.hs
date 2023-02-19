filePath = "../strings.txt"

lDistance :: Eq a => [a] -> [a] -> Int
lDistance [] t = length t
lDistance s [] = length s
lDistance (a : s') (b : t') =
    if a == b
        then lDistance s' t'
        else
            1
                + minimum
                    [ lDistance (a : s') t'
                    , lDistance s' (b : t')
                    , lDistance s' t'
                    ]

main :: IO ()
main = do
    strings <- lines <$> readFile filePath

    let pairs = filter (uncurry (/=)) $ (,) <$> strings <*> strings
        dists = uncurry lDistance <$> pairs

    print $ minimum dists
