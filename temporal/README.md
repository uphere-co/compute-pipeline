# temponym extraction viewer

This program takes a directory as an input parameter and annotate temporal expressions in
all the text files with extension `.maintext`.

Build
-----
```
$ nix-shell shell.nix --argstr uphere-nix-overlay (uphere-nix-overlay directory) --argstr autoencode (autoencode directory) --argstr symbolic (symbolic directory) --argstr textview (textview directory) --argstr HCoreNLP (HCoreNLP directory)
$ ghc sutime.hs
```

Example use
-----------
```
$ ./sutime --dir  ~/repo/workspace/nyt/00
Loading POS tagger from edu/stanford/nlp/models/pos-tagger/english-left3words/english-left3words-distsim.tagger ... done [1.1 sec].
Initializing JollyDayHoliday for SUTime from classpath edu/stanford/nlp/models/sutime/jollyday/Holidays_sutime.xml as sutime.binder.1.
===========================================================
file: 00b95845874f29f4ec61e16b7478b311c3334732e293a9a4be8d3180a93c07e4.maintext
date: 2014-11-03
-----------------------------------------------------------
156     171          the final weeks    <TIMEX3 tid="t1" type="DURATION" value="PXW">the final weeks</TIMEX3>
183     190                  Tuesday    <TIMEX3 tid="t2" type="DATE" value="2014-11-04">Tuesday</TIMEX3>
757     770            midday Sunday    <TIMEX3 tid="t3" type="TIME" value="2014-11-09T12:00">midday Sunday</TIMEX3>
863     870                  Oct. 15    <TIMEX3 tid="t4" type="DATE" value="2014-10-15">Oct. 15</TIMEX3>
1593    1607          the final days    <TIMEX3 tid="t5" type="DURATION" value="PXD">the final days</TIMEX3>
1944    1948                    2008    <TIMEX3 tid="t6" type="DATE" value="2008">2008</TIMEX3>
1967    1971                    2010    <TIMEX3 tid="t7" type="DATE" value="2010">2010</TIMEX3>
1989    2004         two years later    <TIMEX3 tid="t8" type="DATE" value="2016-11-03">two years later</TIMEX3>
2052    2061               this year    <TIMEX3 tid="t9" type="DATE" value="2014">this year</TIMEX3>
2120    2127                 Oct. 15    <TIMEX3 tid="t10" type="DATE" value="2014-10-15">Oct. 15</TIMEX3>
2300    2307                 Oct. 15    <TIMEX3 tid="t11" type="DATE" value="2014-10-15">Oct. 15</TIMEX3>
2519    2537      the past two weeks    <TIMEX3 beginPoint="t12" endPoint="t0" tid="t13" type="DURATION" value="P2W">the past two weeks</TIMEX3>
2539    2552           Two years ago    <TIMEX3 tid="t14" type="DATE" value="2012-11-03">Two years ago</TIMEX3>
2713    2720                 Oct. 22    <TIMEX3 tid="t15" type="DATE" value="2014-10-22">Oct. 22</TIMEX3>
3291    3298                 October    <TIMEX3 tid="t16" type="DATE" value="2014-10">October</TIMEX3>
3336    3340                    2010    <TIMEX3 tid="t17" type="DATE" value="2010">2010</TIMEX3>
3733    3751      the past two weeks    <TIMEX3 beginPoint="t18" endPoint="t0" tid="t19" type="DURATION" value="P2W">the past two weeks</TIMEX3>
-----------------------------------------------------------
Democrats in the House of Representatives have poured more than a half a million

 dollars from their own campaigns into their colleagues’ tight races during the
                                                                            ----
final weeks leading to Tuesday, more than twice the amount their Republican coun
-----------            -------
terparts have given.
                                                                             
...
```