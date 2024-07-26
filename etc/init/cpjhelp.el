;; cpjh help
(defconst my/emacs-help
"EMACS

<ROPT>-…	   			MacOS alt keys
C-x 8 …					Emacs alt keys

M-X						extended command (buffer)
M-TAB					completion at point

C-TAB | C-x ← | →		switch buffer
C-k | C-S-DEL			kill eol | line
C-DEL | (fn)-C-BS 		kill word ← →
C-g						abort action
C-s | C-r				search forward | back
C-z						undo
C-x h					select all text
M-q | M-S-q				fill | un-fill paragraph
M-|						pipe region
M-;						comment (do-what-I-mean)
C-l						re-centre top
C-M-l					reposition window
M-r						move to top
C-SPC					mark | un-mark
C-x a g					define abbreviation
C-x n n	| C-x n w		narrow | widen buffer
C-x x g					revert buffer (quickly)
C-x x t					toggle truncated lines
C-x C-q					toggle read-only mode

C-q C-l      			<FF>
C-x [ | C-x ]			<FF> bk | fwd

M-:						evaluate (interactive)
C-M-;					evaluate region
C-x C-e					evaluate last standard expression (sexp)

C-u 0 C-M-\\   			remove indent
C-u C-x r N				number lines


ORG-MODE

S-TAB					cycle visibility
C-'						cycle agenda
C-c a					agenda
C-c c					capture
C-c l					store link
C-c C-l					edit link
C-c C-s					schedule
C-c C-x C-a				archive
S-←   | S-→ ↑↓ 			status
M-←   | M-→ ↑↓ 			heading
M-↑   | M-↓				move subtree
M-S-↑ | M-S-↓			move item
C⏎    | M-⏎				new head / item
M-S-⏎					new TODO / ☑︎
C-c C-c					tick ☑︎ / edit tag
C-u C-c C-c				adds ☑︎ to list
C-c C-t					TODO / DONE
C-c / t					TODO tree
C-c * | C-c -			headline | item
C-c ^					sort
C-c C-q					tag

C-c TAB					toggle table column width


Calendar

C-<spc> ... M-=			number of days between mark and point
")
(kf-gen-displayer my/emacs-help "Display Emacs help." "*Emacs cheat-sheet*")

(defconst Hanke-Henry
"                                Hanke-Henry Permanent Calendar (HHPC)

    January                         February                        March
    Mo  Tu  We  Th  Fr  Sa  Su      Mo  Tu  We  Th  Fr  Sa  Su      Mo  Tu  We  Th  Fr  Sa  Su
     1   2   3   4   5   6   7               1   2   3   4   5                       1   2   3
     8   9  10  11  12  13  14       6   7   8   9  10  11  12       4   5   6   7   8   9  10
    15  16  17  18  19  20  21      13  14  15  16  17  18  19      11  12  13  14  15  16  17
    22  23  24  25  26  27  28      20  21  22  23  24  25  26      18  19  20  21  22  23  24
    29  30                          27  28  29  30                  25  26  27  28  29  30  31
    --------------------------      --------------------------      --------------------------
    April                           May                             June
    Mo  Tu  We  Th  Fr  Sa  Su      Mo  Tu  We  Th  Fr  Sa  Su      Mo  Tu  We  Th  Fr  Sa  Su
     1   2   3   4   5   6   7               1   2   3   4   5                       1   2   3
     8   9  10  11  12  13  14       6   7   8   9  10  11  12       4   5   6   7   8   9  10
    15  16  17  18  19  20  21      13  14  15  16  17  18  19      11  12  13  14  15  16  17
    22  23  24  25  26  27  28      20  21  22  23  24  25  26      18  19  20  21  22  23  24
    29  30                          27  28  29  30                  25  26  27  28  29  30  31
    --------------------------      --------------------------      --------------------------
    July                            August                          September
    Mo  Tu  We  Th  Fr  Sa  Su      Mo  Tu  We  Th  Fr  Sa  Su      Mo  Tu  We  Th  Fr  Sa  Su
     1   2   3   4   5   6   7               1   2   3   4   5                       1   2   3
     8   9  10  11  12  13  14       6   7   8   9  10  11  12       4   5   6   7   8   9  10
    15  16  17  18  19  20  21      13  14  15  16  17  18  19      11  12  13  14  15  16  17
    22  23  24  25  26  27  28      20  21  22  23  24  25  26      18  19  20  21  22  23  24
    29  30                          27  28  29  30                  25  26  27  28  29  30  31
    --------------------------      --------------------------      --------------------------
    October                         November                        December
    Mo  Tu  We  Th  Fr  Sa  Su      Mo  Tu  We  Th  Fr  Sa  Su      Mo  Tu  We  Th  Fr  Sa  Su
     1   2   3   4   5   6   7               1   2   3   4   5                       1   2   3
     8   9  10  11  12  13  14       6   7   8   9  10  11  12       4   5   6   7   8   9  10
    15  16  17  18  19  20  21      13  14  15  16  17  18  19      11  12  13  14  15  16  17
    22  23  24  25  26  27  28      20  21  22  23  24  25  26      18  19  20  21  22  23  24
    29  30                          27  28  29  30                  25  26  27  28  29  30  31
    --------------------------      --------------------------      --------------------------
                                                                    Xtra
                                                                    Mo  Tu  We  Th  Fr  Sa  Su
                                                                     1   2   3   4   5   6   7")

(kf-gen-displayer Hanke-Henry "Display Hanke-Henry Permanent Calendar." "*HHPC*")
(defalias 'hanke 'Hanke-Henry)

(defconst british-currency
"BRITISH CURRENCY

PRE-DECIMAL £-s-d
12 pence (d) = 1 shilling (s)
20 shillings (s) = 1 pound (£)

'/-' NOTATION
1/1/1 = 1£ 1s 1d
1/- = 1s 0d

DECIMAL (1971)
100 pence (p) = 1 pound (£)

CANADA (1859)
100 cents (¢) = 1 dollar ($)

NOTES

The abbreviation for the old penny, d, was derived from the Roman
denarius, and the abbreviation for the shilling, s, from the
Roman solidus. The shilling was also denoted by the slash symbol,
also called a solidus for this reason, which was originally an
adaptation of the long s. The symbol '£', for the pound, is
derived from the first letter of the Latin word for pound, libra.

The sovereign is a British gold coin with a nominal value of one
pound sterling (£1) and contains 0.2354 troy oz of pure gold.
Struck since 1817, it was originally a circulating coin that was
accepted in Britain and elsewhere in the world; it is now
a bullion coin and is sometimes mounted in jewellery. In
addition, circulation strikes and proof examples are often
collected for their numismatic value. In most recent years, it
has borne the design of Saint George and the Dragon on the
reverse; the initials (B P) of the designer, Benedetto Pistrucci,
are visible to the right of the date.

The guinea (/ˈɡɪniː/; commonly abbreviated gn., or gns. in
plural) was a coin, minted in Great Britain between 1663 and
1814, that contained approximately one-quarter of an ounce of
gold. The name came from the Guinea region in West Africa, from
where much of the gold used to make the coins was sourced. It is
valued at one pound and one shilling (21 shillings, £1.05 in
decimal notation).

The silver crown was a denomination of sterling coinage worth 1⁄4
of one pound, or 5 shillings, or 60 (old) pence.

A common slang term for the pound unit is 'quid' (singular and
plural). The term may have come from Latin quid via the common
phrase quid pro quo, literally, 'what for what'.")

(kf-gen-displayer british-currency "British and Canadian Currency." "*currency*")

(defconst measurements
"UNITS OF MEASURE

LENGTH											AREA
12 inches			=	1 foot						144 sq. inches		=	1 square foot
3 feet				=	1 yard						9 sq. feet			=	1 square yard
22 yards			=	1 chain						4840 sq. yards		=	1 acre
10 chains			=	1 furlong					64 acres			=	1 city block
8 furlongs			=	1 mile						640 acres			=	1 square mile
5280 feet			=	1 mile						1 sq. mile			=	1 section
1760 yards			=	1 mile						36 sections			=	1 township
													1 range				=	1 township E-W (6 mi.)
VOLUME
1728 cu. inches		=	1 cubic foot
27 cu. feet			=	1 cubic yard

CAPACITY (DRY) US				   					CAPACITY (LIQUID)
2 pints				=	1 quart						20 fluid ounces (UK)=	1 pint
8 quarts			=	1 peck						16 fluid ounces (US)=	1 pint
4 pecks				=	1 bushel					4 gills				=	1 pint
													2 pints				=	1 quart
													4 quarts			=	1 gallon (8 pints)

MASS												TROY WEIGHTS
437.5 grains		=	1 ounce						24 grains			=	1 pennyweight
16 ounces			=	1 pound (7000 grains)		20 pennyweights		=	1 ounce (480 grains)
14 pounds			=	1 stone						12 ounces			=	1 pound (5760 grains)
8 stones (112 lbs)	=	1 hundredweight [cwt] (UK)
100 pounds			=	1 hundredweight [cwt] (US)
20 cwt				=	1 ton (2240 pounds) (UK)
20 cwt				=	1 ton (2000 pounds) (US)

APOTHECARIES' MEASURES								APOTHECARIES' WEIGHTS
20 minims			=	1 fl.scruple		   		20 grains			=	1 scruple
3 fl.scruples		=	1 fl.drachm (dram)			3 scruples	   		=	1 drachm
8 fl.drachms (drams)=	1 fl.ounce					8 drachms			=	1 ounce (480 grains)
20 fl.ounces (UK)	=	1 pint						12 ounces			=	1 pound (5760 grains)
16 fl.ounces (US)	=	1 pint


OBSCURE MEASUREMENTS

LENGTH
1 hand				=	4 in.		(17 hands is a good horse)
1 span				=	9 in.
1 cubit				=	18 in.		(originally 21.8 in. / 18.3 in. / 17.5 in.)
1 stadium			=	202 yds.
1 Roman mile		=	5000 ft.
1 league			=	3 mi.

LENGTH (SURVEY)
1 link				=	0.66 ft
1 pole (rod)		=	25 links (5.5 yds.)
1 chain				=	100 links

LENGTH (NAUTICAL)
1 fathom			=	6 ft.
1 cable				=	100 fathoms (600 ft.)
1 nau. mile			=	1.15 miles

AREA
1 rood				=	1,210 sq. yards

MASS
1 oz.				=	2 shekels


IMPERIAL LIQUID MEASURES

+---------+-----------+-----------+-----------+------------+-----------+-----------+
|         |           |           |           | 1/2 fl oz  |   1 tbsp  |   3 tsp   |
+---------+-----------+-----------+-----------+------------+-----------+-----------+
|         |           |           |   1/8 cup |   1 fl oz  |   2 tbsp  |   6 tsp   |
+---------+-----------+-----------+-----------+------------+-----------+-----------+
|         |           |           |   1/4 cup |   2 fl oz  |   4 tbsp  |   12 tsp  |
+---------+-----------+-----------+-----------+------------+-----------+-----------+
|         |           |           |   1/2 cup |   4 fl oz  |   8 tbsp  |   24 tsp  |
+---------+-----------+-----------+-----------+------------+-----------+-----------+
|         |  1/4 qt   |  1/2 pt   |    1 cup  |   8 fl oz  |  16 tbsp  |   48 tsp  |
+---------+-----------+-----------+-----------+------------+-----------+-----------+
|         |  1/2 qt   |   1 pt    |    2 cups |  16 fl oz  |           |           |
+---------+-----------+-----------+-----------+------------+-----------+-----------+
| 1/4 gal |   1 qt    |   2 pt    |    4 cups |  32 fl oz  |           |           |
+---------+-----------+-----------+-----------+------------+-----------+-----------+
| 1/2 gal |   2 qt    |   4 pt    |    8 cups |  64 fl oz  |           |           |
+---------+-----------+-----------+-----------+------------+-----------+-----------+
| 1 gal   |   4 qt    |   8 pt    |   16 cups | 128 fl oz  |           |           |
+---------+-----------+-----------+-----------+------------+-----------+-----------+


CANADIAN MEASUREMENTS

| Imperial      | Symbol | CA     | UK                  | US                  |
|---------------|--------|--------|---------------------|---------------------|
| pint          | pt     |        | 20 fl oz   | 568 ml | 16 fl oz   | 473 ml |
| cup           | c      | 250 ml | 10 fl oz   | 284 ml | 8  fl oz   | 237 ml |
| tumbler       |        |        | 6.6 fl oz  | 190 ml | 8  fl oz   | 235 ml |
|               |        |        |            |        |            |        |
| 60            |        | 1.75 L | 62 fl oz   |        | 59 fl oz   |        |
| 40            |        | 1.14 L | 40 fl oz   |        | 39 fl oz   |        |
| bottle (26)   |        | 750 ml | 26 fl oz   |        | 25 fl oz   |        |
| soft drink    |        | 284 ml | 15.5 fl oz | 440 ml | 20 fl oz   | 591 ml |
| mickey        |        | 375 ml | 13.2 fl oz | 390 ml | 12.7 fl oz | 380 ml |
| beer          |        | 341 ml | 12 fl oz   | 341 ml | 12 fl oz   | 355 ml |
| wineglass     | wgf    |        |            |        | 2 fl oz    |  59 ml |
| teacup (gill) | tcf    |        | 5 fl oz    | 142 ml | 4 fl oz    | 118 ml |
| shot          | jig    |        | 1.5 fl oz  |  43 ml | 1.5 fl oz  |  44 ml |
")

(kf-gen-displayer measurements "Units of measurement." "*measurements*")

;; Local Variables:
;; tab-width: 4
;; End:
