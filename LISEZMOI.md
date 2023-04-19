Ce programme permet de déterminer l'ensemble des anagrammes plausibles
d'un nom entré par l'utilisateur. Il se sert pour cela d'une base de 
données constituée de plus de 30.000 prénoms qui vont lui permettre de générer 
dans un premier temps les différents prénoms des anagrammes, les patronymes 
étant, quant à eux, générés avec les lettres restantes.

Evitez d'utiliser des caractères accentués car ces derniers ne seront pas
pris en compte dans la génération d'anagrammes. Vous pouvez en revanche 
entrer votre nom en utilisant des minuscules, des majuscules ou les deux.

Le programme est livré avec un fichier Makefile.

**Compilation**<br>
make all

**Exécution**<br>
./anagrammes

**Netoyage de l'archive des fichiers de compilation**<br>
make clean
