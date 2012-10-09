#
# "objdump -d -l angbnd64 > test.in" to create the input file!
#
# result:
#
#
# 804af8c@z-util.c:143@suffix,15@144,36@145,60@148,72@151
# 804b03c@z-util.c:159@prefix,115@161,120@164,292@165,347@168
# <hex-address>@filename:line-number>@function name,[<offset>@<line-number>][,<offset>@<line-number>]
#
function tohex(num)
{
   return sprintf("%x", num);
}

function todec(num)
{
   result=0;
   base=1;
   for (i=length(num); i > 0; i--)
   {
      byte=tolower(substr(num, i, 1));
      val=0;
      if (byte == "1") val=1;
      if (byte == "2") val=2;
      if (byte == "3") val=3;
      if (byte == "4") val=4;
      if (byte == "5") val=5;
      if (byte == "6") val=6;
      if (byte == "7") val=7;
      if (byte == "8") val=8;
      if (byte == "9") val=9;
      if (byte == "a") val=10;
      if (byte == "b") val=11;
      if (byte == "c") val=12;
      if (byte == "d") val=13;
      if (byte == "e") val=14;
      if (byte == "f") val=15;
      result += val * base;
      base *= 16;
   }
   return (result);
}

BEGIN {
         prevfuncname="XX";
      }
# new function? save the name and starting address
/>:/  {
         junk=split($0, veld, " ");
         start=veld[1];
         junk=gsub("<", "", veld[2]);
         junk=gsub(">:", "", veld[2]);
         funcname=veld[2];
      }
# new line? save info and signal a print
/^\// {
         num=split($0, veld, "/");
         line=veld[num];
         do_print=1;
      }
/^ /  {
         junk=split($0, veld, " ");
         if (do_print==1)
         {
            junk=gsub(":$", "", veld[1]);
            if (funcname != prevfuncname)
            {
               junk=gsub(":","@",line);
               printf "%s@%s@%s\n", veld[1], line, funcname;
               prevfuncname=funcname;
               start=todec(veld[1]);
            }
            else
            {
               junk=split(line, veld2, ":");
               printf "@%d@%s\n", todec(veld[1])-start, veld2[2];
            }
            do_print=0;
         } 
      }
