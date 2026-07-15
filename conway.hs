--ugh, conway's game of life maybe eventually


data Cell = Dead | Alive
    deriving Show


type Grid = [[Cell]]

zeros :: Grid
zeros =
    [ [Dead ,Dead ,Dead ,Dead ,Dead ]
    , [Dead ,Dead ,Dead ,Dead ,Dead ]
    , [Dead ,Dead ,Dead ,Dead ,Dead ]
    , [Dead ,Dead ,Dead ,Dead ,Dead ]
    , [Dead ,Dead ,Dead ,Dead ,Dead ]
    ]


checkerboard :: Grid
checkerboard =
    [ [Dead ,Alive,Dead ,Alive,Dead ]
    , [Alive,Dead ,Alive,Dead ,Alive]
    , [Dead ,Alive,Dead ,Alive,Dead ]
    , [Alive,Dead ,Alive,Dead ,Alive]
    , [Dead ,Alive,Dead ,Alive,Dead ]
    ]

dispCell :: Cell -> Char
dispCell Dead  = '.'
dispCell Alive = '#'


dispGrid :: Grid -> IO ()
dispGrid g =
    mapM_ putStrLn 
        [ [ dispCell cell
          | cell <- row
          ]
        | row <- g
        ]

cellAt :: Grid -> Int -> Int -> Cell

cellAt g x y =
    Dead
