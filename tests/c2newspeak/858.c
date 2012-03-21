// C99-style array initializers. Those two ones should be the same.

int a[10] =
    { [7] = 7
    , [6] = 6
    , [8] = 8
    , [0] = 0
    , [9] = 9
    , [2] = 2
    , [4] = 4
    , [3] = 3
    , [5] = 5
    , [1] = 1
    };

int b[10] =
    { 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    };
