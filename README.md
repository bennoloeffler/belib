# belib

A Clojure library.
Tools and helper functions collected by 
Benno Löffler. Short: BEL. Therefore: belib...

## Usage

just use it from clojars. See current version.
```
[belib "0.1.0]
```

just install it locally by
```
lein install
```

Checkout to "projects/belib" and  
use lein checkout dependencies.
CD into target project, e.g. projects/x
THIS DOES NOT WORK WITH CURSIVE.
```
cd projects/x
mkdir checkouts
cd checkouts
ln -s ../../belib
```
just checkout to projects folder,  
make a symbolic link from  
src of app into src from belib
```
cd src/clj
ln -s ../../../belib/src/belib
```

finally, it looks like
```
re-pipe (the app)
└── src
      ├── clj
      │  ├── belib -> ../../../belib/src/belib
      │  └── re_pipe
      ├── cljc
      │  └── re_pipe
      └── cljs
         └── re_pipe
    
```

## License

Copyright © 2023 Benno Löffler

```
        DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE 
                    Version 2, December 2004 

 Copyright (C) 2004 Sam Hocevar <sam@hocevar.net> 

 Everyone is permitted to copy and distribute verbatim or modified 
 copies of this license document, and changing it is allowed as long 
 as the name is changed. 

            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE 
   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION 

  0. You just DO WHAT THE FUCK YOU WANT TO.
```
