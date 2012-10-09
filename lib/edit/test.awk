# I:<level>1:<mana>1:<chance>22:<minfail>2
/^I:/ { junk=gsub("<[a-z]*>","",$0);
        junk=split($0, veld, ":");
        printf "veld1:%s veld2:%s veld3:%s veld4:%s\n", veld[1], veld[2], veld[3], veld[4];
      }

        
