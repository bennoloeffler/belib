# belib

A personal clojure library.  
Tools and helper functions  
collected by Benno Löffler.  
Short: BEL. So: belib

## Usage

1. Just use it from clojars. 
See current version.
```
lein search belib
```

Examples:
```
[belib "X.Y.Z]
[belib "X.Y.Z-SNAPSHOT]
```

Or 
2. checkout and install it locally by

```
cd ~/projects
git clone https://github.com/bennoloeffler/belib.git
cd belib
lein install
```
And maybe  

3. Use lein checkout dependencies  
in the target project x,  
if you would like to adapt belib:
```
cd ~/projects/x
mkdir checkouts
cd checkouts
ln -s ../../belib
```

finally, it looks like  
in your **projects** folder:
```
x (the app)
│
├── checkouts
│         └── belib -> ../../belib 
└── src
      ├── clj
      │  └── x
      ├── cljc
      │  └── x
      └── cljs
         └── x
belib
└── src
      ├── cljc
      └── clj    
```
Should work with repl.  
**In order to make it work with cursive,
import the project.clj of belib in the
target app x.** by clicking on it and importing it.  
**THEN, you need to LOAD THE SOURCE FILES manually**.  
Otherwise, the locally installed jar will be used! 
If a target project of belib in intellij exists,    
it seems to be overwritten and broken afterwards...  

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
