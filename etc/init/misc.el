;; https://svn.red-bean.com/repos/kfogel/trunk/.emacs

;;; Setting modes based on filenames:
(add-to-list 'auto-mode-alist '("\\.mnu$" . food-menu-mode))
(add-to-list 'auto-mode-alist '("\\.pl$" . perl-mode))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.pm$" . perl-mode))
(add-to-list 'auto-mode-alist '("\\.cgi$" . perl-mode))
(add-to-list 'auto-mode-alist '("\\.sgml$" . text-mode))
(add-to-list 'auto-mode-alist '("\\.ss$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.s?html?\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.scm$" . scheme-mode))
(add-to-list 'auto-mode-alist '("logv\\.out$" . kf-changelog-mode))
(add-to-list 'auto-mode-alist '("\\.patch$" . text-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . text-mode))


;;; On-demand help panels for obscure topics. ;;;

(defun kf-display-something-maybe-big (contents &optional title)
  "Display string CONTENTS in a buffer named TITLE."
  (let ((buf (get-buffer-create (or title "*STUFF*")))
        (win nil)
        (lines nil))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (insert contents)
      (goto-char (point-min))
      (setq lines (count-lines (point-min) (point-max)))
    (setq win (display-buffer buf))
    (when (> lines (window-text-height win))
      (select-window win))
	(help-mode) )))

(defmacro kf-gen-displayer (txt-sym fn-doc-str buf-name &optional fn-alias)
  "Generate an interactive function with the same symbol name as TXT-SYM,
whose doc string is FN-DOC-STR, and that when invoked displays TXT-SYM
in a buffer named BUF-NAME using `display-buffer'."
  (declare (indent 2))
  `(progn
     (defun ,txt-sym ()
       ,fn-doc-str
       (interactive)
       (kf-display-something-maybe-big ,txt-sym ,buf-name))
     (when (or (not (boundp ',fn-alias)) (not (eq nil ,fn-alias)))
       (defalias ',fn-alias ',txt-sym))))


(defconst kf-ascii
  "
       Decimal - Character

       |  0 NUL|  1 SOH|  2 STX|  3 ETX|  4 EOT|  5 ENQ|  6 ACK|  7 BEL|
       |  8 BS |  9 HT | 10 NL | 11 VT | 12 NP | 13 CR | 14 SO | 15 SI |
       | 16 DLE| 17 DC1| 18 DC2| 19 DC3| 20 DC4| 21 NAK| 22 SYN| 23 ETB|
       | 24 CAN| 25 EM | 26 SUB| 27 ESC| 28 FS | 29 GS | 30 RS | 31 US |
       | 32 SP | 33  ! | 34  \" | 35  # | 36  $ | 37  % | 38  & | 39  ' |
       | 40  \( | 41  \) | 42  * | 43  + | 44  , | 45  - | 46  . | 47  / |
       | 48  0 | 49  1 | 50  2 | 51  3 | 52  4 | 53  5 | 54  6 | 55  7 |
       | 56  8 | 57  9 | 58  : | 59  ; | 60  < | 61  = | 62  > | 63  ? |
       | 64  @ | 65  A | 66  B | 67  C | 68  D | 69  E | 70  F | 71  G |
       | 72  H | 73  I | 74  J | 75  K | 76  L | 77  M | 78  N | 79  O |
       | 80  P | 81  Q | 82  R | 83  S | 84  T | 85  U | 86  V | 87  W |
       | 88  X | 89  Y | 90  Z | 91  [ | 92  \\ | 93  ] | 94  ^ | 95  _ |
       | 96  ` | 97  a | 98  b | 99  c |100  d |101  e |102  f |103  g |
       |104  h |105  i |106  j |107  k |108  l |109  m |110  n |111  o |
       |112  p |113  q |114  r |115  s |116  t |117  u |118  v |119  w |
       |120  x |121  y |122  z |123  { |124  | |125  } |126  ~ |127 DEL|

       Hexadecimal - Character

       | 00 NUL| 01 SOH| 02 STX| 03 ETX| 04 EOT| 05 ENQ| 06 ACK| 07 BEL|
       | 08 BS | 09 HT | 0A NL | 0B VT | 0C NP | 0D CR | 0E SO | 0F SI |
       | 10 DLE| 11 DC1| 12 DC2| 13 DC3| 14 DC4| 15 NAK| 16 SYN| 17 ETB|
       | 18 CAN| 19 EM | 1A SUB| 1B ESC| 1C FS | 1D GS | 1E RS | 1F US |
       | 20 SP | 21  ! | 22  \" | 23  # | 24  $ | 25  % | 26  & | 27  ' |
       | 28  \( | 29  \) | 2A  * | 2B  + | 2C  , | 2D  - | 2E  . | 2F  / |
       | 30  0 | 31  1 | 32  2 | 33  3 | 34  4 | 35  5 | 36  6 | 37  7 |
       | 38  8 | 39  9 | 3A  : | 3B  ; | 3C  < | 3D  = | 3E  > | 3F  ? |
       | 40  @ | 41  A | 42  B | 43  C | 44  D | 45  E | 46  F | 47  G |
       | 48  H | 49  I | 4A  J | 4B  K | 4C  L | 4D  M | 4E  N | 4F  O |
       | 50  P | 51  Q | 52  R | 53  S | 54  T | 55  U | 56  V | 57  W |
       | 58  X | 59  Y | 5A  Z | 5B  [ | 5C  \\ | 5D  ] | 5E  ^ | 5F  _ |
       | 60  ` | 61  a | 62  b | 63  c | 64  d | 65  e | 66  f | 67  g |
       | 68  h | 69  i | 6A  j | 6B  k | 6C  l | 6D  m | 6E  n | 6F  o |
       | 70  p | 71  q | 72  r | 73  s | 74  t | 75  u | 76  v | 77  w |
       | 78  x | 79  y | 7A  z | 7B  { | 7C  | | 7D  } | 7E  ~ | 7F DEL|

       Octal - Character

       |000 NUL|001 SOH|002 STX|003 ETX|004 EOT|005 ENQ|006 ACK|007 BEL|
       |010 BS |011 HT |012 NL |013 VT |014 NP |015 CR |016 SO |017 SI |
       |020 DLE|021 DC1|022 DC2|023 DC3|024 DC4|025 NAK|026 SYN|027 ETB|
       |030 CAN|031 EM |032 SUB|033 ESC|034 FS |035 GS |036 RS |037 US |
       |040 SP |041  ! |042  \" |043  # |044  $ |045  % |046  & |047  ' |
       |050  \( |051  \) |052  * |053  + |054  , |055  - |056  . |057  / |
       |060  0 |061  1 |062  2 |063  3 |064  4 |065  5 |066  6 |067  7 |
       |070  8 |071  9 |072  : |073  ; |074  < |075  = |076  > |077  ? |
       |100  @ |101  A |102  B |103  C |104  D |105  E |106  F |107  G |
       |110  H |111  I |112  J |113  K |114  L |115  M |116  N |117  O |
       |120  P |121  Q |122  R |123  S |124  T |125  U |126  V |127  W |
       |130  X |131  Y |132  Z |133  [ |134  \\ |135  ] |136  ^ |137  _ |
       |140  ` |141  a |142  b |143  c |144  d |145  e |146  f |147  g |
       |150  h |151  i |152  j |153  k |154  l |155  m |156  n |157  o |
       |160  p |161  q |162  r |163  s |164  t |165  u |166  v |167  w |
       |170  x |171  y |172  z |173  { |174  | |175  } |176  ~ |177 DEL|
       "
  "The ASCII character tables.")
(kf-gen-displayer kf-ascii
                  "Display the ASCII character table in its own buffer."
                  "*ASCII*")


(defconst kf-datetime-formats
  "See:

  * http://pleac.sourceforge.net/pleac_python/datesandtimes.html
  * http://docs.python.org/library/time.html
  * http://docs.python.org/library/datetime.html
  * http://www.python.org/doc/2.5.2/lib/datetime-tzinfo.html
  * http://uswaretech.com/blog/2009/02/understanding-datetime-tzinfo-timedelta-timezone-conversions-python/

  From http://docs.python.org/library/time.html#time.strftime:

    %a      Locale's abbreviated weekday name.
    %A      Locale's full weekday name.
    %b      Locale's abbreviated month name.
    %B      Locale's full month name.
    %c      Locale's appropriate date and time representation.
    %d      Day of the month as a decimal number [01,31].
    %H      Hour (24-hour clock) as a decimal number [00,23].
    %I      Hour (12-hour clock) as a decimal number [01,12].
    %j      Day of the year as a decimal number [001,366].
    %m      Month as a decimal number [01,12].
    %M      Minute as a decimal number [00,59].
    %p      Locale's equivalent of either AM or PM. (1)
    %S      Second as a decimal number [00,61].     (2)
    %U      Week number of the year (Sunday as the first day of the week)
            as a decimal number [00,53]. All days in a new year preceding
            the first Sunday are considered to be in week 0.  (3)
    %w      Weekday as a decimal number [0(Sunday),6].
    %W      Week number of the year (Monday as the first day of the week)
            as a decimal number [00,53]. All days in a new year preceding
            the first Monday are considered to be in week 0.  (3)
    %x      Locale's appropriate date representation.
    %X      Locale's appropriate time representation.
    %y      Year without century as a decimal number [00,99].
    %Y      Year with century as a decimal number.
    %Z      Time zone name (no characters if no time zone exists).
    %%      A literal '%' character.

    Notes:

      1) When used with the strptime() function, the %p directive only
         affects the output hour field if the %I directive is used to
         parse the hour.
      2) The range really is 0 to 61; this accounts for leap seconds
         and the (very rare) double leap seconds.
      3) When used with the strptime() function, %U and %W are only
         used in calculations when the day of the week and the year
         are specified.

    Here is an example, a format for dates compatible with that
    specified in the RFC 2822 Internet email standard. [1]

      >>> from time import gmtime, strftime
      >>> strftime('%a, %d %b %Y %H:%M:%S +0000', gmtime())
      'Thu, 28 Jun 2001 14:17:15 +0000'

== Date codes, for NS (NextStep) Foundation Classes  ==
==   (and possibly the Unix date command as well)    ==

   %a     abbreviated weekday name
   %A     full weekday name
   %b     abbreviated month name
   %B     full month name
   %c     shorthand for %X %x, the locale format for date and time
   %d     day of the month as a decimal number (01-31)
   %e     same as %d but does not print the leading 0 for days 1 through 9
   %F     milliseconds as a decimal number (000 - 999)
   %H     hour based on a 24-hour clock as a decimal number (00-23)
   %I     hour based on a 12-hour clock as a decimal number (01-12)
   %j     day of the year as a decimal number (001-366)
   %m     month as a decimal number (01-12)
   %M     minute as a decimal number (00-59)
   %p     AM/PM designation for the locale
   %S     second as a decimal number (00-61)
   %w     weekday as a decimal number (0-6), where Sunday is 0
   %x     date using the date representation for the locale
   %X     time using the time representation for the locale
   %y     year without century (00-99)
   %Y     year with century (such as 1990)
   %Z     time zone abbreviation (such as PDT)
   %z     time zone offset in hours and minutes from GMT (HHMM)
   %%     a '%' character, of course
"
  "Date and time formats for various programming languages.")
(kf-gen-displayer kf-datetime-formats
                  "Display date/time format codes in their own buffer"
                  "*Date / Time Formats*")


(defconst kf-radio-alphabet
   "                A - Alpha                  N - November
                B - Bravo                  O - Oscar
                C - Charlie                P - Papa
                D - Delta                  Q - Quebec
                E - Echo                   R - Romeo
                F - Foxtrot                S - Sierra
                G - Golf                   T - Tango
                H - Hotel                  U - Uniform
                I - India                  V - Victor
                J - Juliet                 W - Whiskey
                K - Kilo                   X - X-ray
                L - Lima                   Y - Yankee
                M - Mike                   Z - Zulu"
   "Wear aviator goggles when confirming airline reservation numbers.")
(kf-gen-displayer kf-radio-alphabet
                  "Display the radio alphabet in its own buffer."
                  "*RADIO ALPHABET*")


(defconst kf-stellar-statistics
  "
   The Sun:
        diameter:    1,390,000 km.
        mass:        1.989e30 kg
        temperature: 5800 K (surface), 15,600,000 K (core)
   ---------------------------------------------------------------------

               Distance  Radius    Mass
   Planet      (000 km)   (km)     (kg)   Discoverer   Date
   ---------  ---------  ------  -------  ----------  -----
   Mercury       57,910    2439  3.30e23
   Venus        108,200    6052  4.87e24
   Earth        149,600    6378  5.98e24
   Mars         227,940    3397  6.42e23
   Jupiter      778,330   71492  1.90e27
   Saturn     1,426,940   60268  5.69e26
   Uranus     2,870,990   25559  8.69e25   Herschel    1781
   Neptune    4,497,070   24764  1.02e26   Galle       1846


   Non-Planet   (000 km)   (km)     (kg)   Discoverer   Date
   ---------  ---------  ------  -------  ----------  -----
   Pluto      5,913,520    1160  1.31e22   Tombaugh    1930
   ---------------------------------------------------------------------

   So Earth is about 6 septillion kg (5.98 x 10^24 kg).


   No.  Name      Distance  Radius     Mass  Discoverer   Date
   ---- ---------  --------  ------  -------  ----------  -----
   2062 Aten         144514       0.5   ?      Helin       1976
   3554 Amun         145710       ?     ?      Shoemaker   1986
   1566 Icarus       161269       0.7   ?      Baade       1949
    951 Gaspra       205000       8     ?      Neujmin     1916
   1862 Apollo       220061       0.7   ?      Reinmuth    1932
    243 Ida          270000      35     ?      ?           1880?
   2212 Hephaistos   323884       4.4   ?      Chernykh    1978
      4 Vesta        353400     265  3.0e20    Olbers      1807
      3 Juno         399400     123     ?      Harding     1804
     15 Eunomia      395500     136     ?      De Gasparis 1851
      1 Ceres        413900     466  8.7e20    Piazzi      1801
      2 Pallas       414500     261  3.18e20   Olbers      1802
     52 Europa       463300     156     ?      Goldschmidt 1858
     10 Hygiea       470300     215     ?      De Gasparis 1849
    511 Davida       475400     168     ?      Dugan       1903
    911 Agamemnon    778100      88     ?      Reinmuth    1919
   2060 Chiron      2051900      85     ?      Kowal       1977
   ---------------------------------------------------------------------
"
  "Stats on the Sun, planets and selected asteroids.")
(kf-gen-displayer kf-stellar-statistics
                  "Display some statistics about the solar system."
                  "*Solar System*")


(defconst kf-gnupg-help
  "In Emacs, use `C-c RET C-e' to encrypt+sign from Message Mode
(or use `C-c RET C-s' to just sign without encrypting).

To find/fetch a key:

  gpg --keyserver hkps://KEYSERVER --recv-keys 0xAEA84EDCF01AD86C4701C85C63113AE866587D0A
  gpg --keyserver hkps://KEYSERVER --search-keys some@email.address

Keyservers to try:

  - keyring.debian.org
  - keyserver.ubuntu.com
  - keys.gnupg.net
  - pool.sks-keyservers.net
  - keys.openpgp.org
  - pgp.mit.edu

To send a signed key to a person:

  gpg --armor --output OTHERKEY.signed-by.C5ED8345.asc --export OTHERKEY

To send a signed key to a keyserver:
  gpg --keyserver KEYSERVER --send-key 16A0DE01

To verify a signature in Gnus:

  W s   (`gnus-summary-force-verify-and-decrypt')

To start/stop/restart gpg-agent:

  gpg-connect-agent /bye              # start it, say for SSH to use it
  gpgconf --kill gpg-agent            # stop it (gpg will J-i-T restart it)
  gpg-connect-agent reloadagent /bye  # restart it, in theory, but a)
                                        you don't need to, and b) this
                                        command didn't have the effect I
                                        expected the one time I tried it

To see which recipients a file has been encrypted for:

  gpg --batch --list-packets path/to/file.asc

Batch mode:

  gpg --batch --passphrase-file <passfile> --output <outfile> --decrypt <gpgfile>

Dealing with key errors:

  You might get an error like this:

    gpg: 180713EB6C6E4ED775E277D59836566B272CEF7F: skipped: Unusable public key
    gpg: [stdin]: encryption failed: Unusable public key

  That can happen with an apparently still-current key if the
  signature component of the key has expired.  Inspect like so:

    $ gpg --verbose --list-keys jrandom
    gpg: using pgp trust model
    gpg: Note: signature key 7CF9CA24B08B8032 expired Tue 30 Aug 2022 07:48:48 PM CDT
    pub   rsa4096/9836566B272CEF7F 2021-08-31 [C]
          180713EB6C6E4ED775E277D59836566B272CEF7F
    uid                 [  full  ] J Random <jrandom@example.com>
    sub   rsa4096/7CF9CA24B08B8032 2021-08-31 [S] [expired: 2022-08-31]
    sub   rsa4096/3A140330EBEE2ED6 2021-08-31 [E] [expired: 2022-08-31]
    sub   rsa4096/6616A0179FD153B2 2021-08-31 [A] [expired: 2022-08-31]

  To fix, do this:

    $ gpg --recv-keys 0x9836566B272CEF7F

  You might also get an error like this:

    gpg: 8E8AF6393F237A2E: There is no assurance this key belongs to the named user
    gpg: [stdin]: encryption failed: Unusable public key

  It's due to GPG trust/signature issues that are so intricate
  that I won't go into them here because I don't want to use up
  the remaining blank bits in my .emacs :-(.  The solution I
  used (in at least one case, anyway) was to add both a trust
  level and a signature to the relevant subkey (notice how in the
  error message above, the \"8E8AF6393F237A2E\" matches the subkey
  shown below, rather than matching the main public key).

  One solution is to do something like this:

    > $ gpg --edit-key 5972830CA206DCBA1EF97758D674C7632F4AC0E7
    > pub  rsa4096/2674C7632F4AC0E7
    >      created: 2022-11-21  expires: 2024-11-21  usage: SC
    >      trust: full          validity: unknown
    > sub  rsa4096/8E8AF6393F237A2E
    >      created: 2022-11-21  expires: 2024-11-21  usage: E
    > [ unknown] (1). J. Random <jrandom@example.com>
    >
    > gpg> key 8E8AF6393F237A2E
    >
    > [...]
    >
    > gpg> trust
    > Please decide how far you trust this user to correctly verify other users' keys
    > (by looking at passports, checking fingerprints from different sources, etc.)
    >
    >   1 = I don't know or won't say
    >   2 = I do NOT trust
    >   3 = I trust marginally
    >   4 = I trust fully
    >   5 = I trust ultimately
    >   m = back to the main menu
    >
    > Your decision? 4
    >
    > [...]
    >
    > gpg> sign
    > Your current signature on \"J. Random <jrandom@example.com>\"
    > is a local signature.
    > Do you want to promote it to a full exportable signature? (y/N) y
    >
    > pub  rsa4096/2674C7632F4AC0E7
    >      created: 2022-11-21  expires: 2024-11-21  usage: SC
    >      trust: full          validity: full
    >  Primary key fingerprint: 5972 830C A206 DCBA 1EF9  7758 D674 C763 2F4A C0E7
    >
    >      J. Random <jrandom@example.com>
    >
    > This key is due to expire on 2024-11-21.
    > Are you sure that you want to sign this key with your
    > key \"Karl Fogel <kfogel@example.com>\" (810A75CB5CDE3845)
    >
    > Really sign? (y/N) y

  Note that I had first done 'lsign' instead of 'sign', and that
  worked fine insofar as it solved my problem, but it generated a
  local (non-exportable) signature.  Since I wanted to send the
  signed key to others, I did the dance again with 'sign' before
  exporting.

  See also this thread from Matthias Apitz on gnupg-users@:

  https://lists.gnupg.org/pipermail/gnupg-users/2019-October/thread.html#62955

Various advice from http://ben.reser.org/key-transition.txt.asc:

  The old key was:

  pub   1024D/641E358B 2001-04-12
        Key fingerprint = 42F5 91FD E577 F545 FB40  8F6B 7241 856B 641E 358B

  And the new key is:

  pub   4096R/16A0DE01 2011-01-28
        Key fingerprint = 19BB CAEF 7B19 B280 A0E2  175E 62D4 8FAD 16A0 DE01

  To fetch the full key, you can get it with:

    curl http://ben.reser.org/benreser.asc | gpg --import -

  Or, to fetch my new key from a public key server, you can simply do:

    gpg --keyserver hkp://KEYSERVER --recv-key 16A0DE01

  If you already know my old key, you can now verify that the new key is
  signed by the old one:

    gpg --check-sigs 16A0DE01

  If you don't already know my old key, or you just want to be double
  extra paranoid, you can check the fingerprint against the one above:

    gpg --fingerprint 16A0DE01

  If you are satisfied that you've got the right key, and the UIDs match
  what you expect, I'd appreciate it if you would sign my key:

    gpg --sign-key 16A0DE01

  Lastly, if you could upload these signatures, i would appreciate it.
  You can either send me an e-mail with the new signatures (if you have
  a functional MTA on your system):

    gpg --armor --export 16A0DE01 | mail -s 'OpenPGP Signatures' ben@reser.org

  Or you can just upload the signatures to a public keyserver directly:

  gpg --keyserver KEYSERVER --send-key 16A0DE01
")
(kf-gen-displayer kf-gnupg-help
                  "You never know when the WWW might be down.  Or Google."
                  "*Because command-line arcana == more security.  Really*"
                  kf-gpg-help)


(defconst kf-principl-help
  "
* Principle

  (n) A doctrine, rule, standard, or law.
  \"The principle of non-violent resistance.\"

* Principal

  (adj) Main, chief, prevailing.
  \"Haste is the principal cause of security failures.\"

  (n) Person or organization holding an important position or role.
  \"The principals met in Slovenia to discuss the matter.\"

  (n) One on whose behalf an agent acts.
  \"The principal, wishing to remain anonymous, sent her agent.\"

  (n) The base investment or sum of money on which interest is paid.
  \"In principle, mortgage borrowers pay more in interest than in principal.\"
  ")
(kf-gen-displayer kf-principl-help
                  "Tired of Googling this one all the time."
                  "*English, the failure-friendly language.*")


(defconst kf-git-help
  "Git trivia that I often need and equally often forget.

To stash / unstash:

  $ git stash
  $ git checkout -b desired-branch-where-the-changes-should-live
  $ git stash-apply

To undo a popped stash that conflicted, so can review it as a diff:

  ### First, here's how you got into that situation:

  $ git stash
  $ git pull
    ## ...receive changes... ##
  $ git stash pop
  Auto-merging FILE.TXT
  CONFLICT (content): Merge conflict in FILE.TXT
  $ git status
  On branch master
  Your branch is up-to-date with 'origin/master'.
  Unmerged paths:
  (use \"git reset HEAD <file>...\" to unstage)
  (use \"git add <file>...\" to mark resolution)

	both modified:   FILE.TXT

  ### Yikes.  Run away!  We want to go back to just before \"git stash pop\".
  ### Fortunately, that is possible:

  $ git reset HEAD .
  $ git stash                    ## Save the un-merged stash pop diff,
                                 ## just in case.
  $ git stash show -p stash@{1}  ## Show diff of the older stash,
                                 ## which git cleverly saved even
                                 ## after the pop, because git knows
                                 ## that stash didn't apply cleanly.
                                 ## Smart git.  Good git.  Nice git.

To clean up a working tree, i.e., put it in its pristine state:

  $ git clean  # maybe with -x and/or -f option

To stage just selected hunks of a file's current diff:

  $ git add -p|--patch FILE

  But since you may have already 'git add'ed the file, you might need
  to unstage selected hunks instead :-).  That's done with:

  $ git reset -p|--patch FILE

To unstage:

  $ git reset FILE

To view the changed files in commits with git log:

  $ git log --name-status
  $ git log --name-only
  $ git log --stat          (shows the most)

To find commit(s) (on a branch) by date:

  $ git rev-list -n 1 --before=\"2013-07-24 00:00\" master

To show commits across all branches:

  $ git log --name-status --all

  (The first option isn't strictly necessary, but I almost always want it.)

To do the equivalent of 'svn cat':

  $ git show rev:filepath

To find out if there are new changes upstream:

  $ git pull --dry-run
      -or-
  $ git remote -v update

To create and apply a git-friendly patch for a specific commit:

  # Create the patch:
  $ git format-patch REF^..REF
  0001-git-commit-msg-first-line-goes-here.patch

  # Preview step 1 -- get stats:
  $ git apply --stat 0001-git-commit-msg-first-line-goes-here.patch

  # Preview step 2 -- dry run:
  $ git apply --check 0001-git-commit-msg-first-line-goes-here.patch

  # Some places (e.g., Emacs) want no sign-off:
  $ git am < 0001-git-commit-msg-first-line-goes-here.patch

  # Other places want sign-off:
  $ git am --signoff < 0001-git-commit-msg-first-line-goes-here.patch

To push a new locally-created branch to a remote origin:

  $ git push origin branch_name:branch_name

  If you want to push to a different remote and push to it repeatedly,
  you can create the remote in .git/config.  For example, I checked
  out git://github.com/codeforamerica/srtracker.git read-only (on the
  remote end, at least).  Then I made new branches locally.  Then I
  wanted to submit pull requests from them.  So I forked to create
  https://github.com/OpenTechStrategies/srtracker.git and
  and pushed my new local branches \"installation-doc-fix\"
  and \"full-text-search\" up to that new repository, by first adding
  this to .gitconfig (modeled on the \"origin\" remote):

    $ git remote add ots git@github.com:OpenTechStrategies/srtracker.git

  Or you could just add the new remote by hand in the .git/config file:

    [remote \"ots\"]
           fetch = +refs/heads/*:refs/remotes/ots/*
           url = git@github.com:OpenTechStrategies/srtracker.git

  Then do:

    $ git push ots full-text-search:full-text-search

To fetch a remote branch after cloning (since clone only gets master):

  $ git checkout -b mybranch origin/mybranch

To see if a branch has been merged into master:

  $ git branch --merged master  # branches merged into master
  $ git branch --merged         # branches merged into current HEAD
  $ git branch --no-merged      # branches not yet merged

  (Use -a to show both local and remote, or -r for remote only.)

To archive a branch, locally and remotely:

  $ git checkout feature-xml
  $ git tag archive/feature-xml feature-xml
  $ git checkout master
  $ git branch -d feature-xml
  $ git push origin :feature-xml
  $ git push --tags

To remove/rename remote branches:

  $ git push origin --delete name_of_the_remote_branch

  # Above is for git >=1.7.  Earlier syntax was:
  $ git push origin :name_of_remote_branch_to_remove

  (If renaming, just rename the branch locally, perhaps with
  `git branch -m local_oldname local_newname`, then push it up
  with `$ git push origin new_name_of_new_local_branch`, and
  finally remove the old name remotely as per above.)

To find out where a repository came from:

  $ git config --get remote.origin.url
    ### local information only, no remote repository contact ###

  $ git remote show origin
    ### this one actually contacts the remote repository ###

To edit the most recent commit message:

  $ git commit --amend

  (If you really *really* need to push it up to the remote repository,
  and you're sure that no one could *possibly* have pushed or pulled
  in the meantime, then 'git push -f origin master' or something like
  that will do the trick.)

To edit an older commit message, enter interactive rebase land:

  $ git rebase -i HEAD~3   ## or whatever ref spec

  # Now change \"pick\" to \"reword\" on the appropriate lines,
  # and for each commit chosen for editing, do...

  $ git commit --amend
  $ git rebase --continue

  # ...when presented with that commit in the rebase cycle.

  Remember to use 'git push --force' to send the reworded branch back
  upstream (and warn your collaborators if necessary).

To squash recent commits into one:

  $ git rebase -i parent-of-first-commit-in-squash-group

  Replace 'pick' on the second and subsequent commit lines with
  'squash' or 'fixup' as described in the 'git-rebase' manual.
  For example, in a repository where commit 91ee0266c is the
  parent of the first squashable commit c0607b5 (which is the one
  whose log message I want to keep), and then a bunch of
  subsequent commits should also be squashed with their log
  messages discarded, then:

    $ git rebase -i 91ee0266c

  and then produce this in the rebase buffer (time flows downward):

    pick c0607b5 Add various fixes and Debian tips to INSTALL.md.
    fixup cde6441 Trying some formatting tricks in INSTALL.md.
    fixup 2508bf6 Those formatting tricks seem to work.
    fixup 535482d Getting closer with the formatting tricks.
    fixup f1a3a11 Yet closer with the formatting tricks.
    fixup c845f16 Even closer with the formatting tricks.
    fixup 9c1a1bd More formatting tricks.
    fixup 27ad1d5 One more formatting trick.
    fixup b904739 Another formatting trick.
    fixup 22e1e83 Last bits of formatting.

To sync with my force-rebased upstream branch:

  $ git fetch remotename
  $ git rebase remotename/branchname
  [... see replay, messy warning, etc ...]
  $ git rebase --skip

  (There's probably a cleaner way to force-pull updates from
  a remote, but I haven't figured it out yet.  You'd think
  'git pull --force' would do this, but apparently not.)

To list all the remote branches:

  $ git ls-remote --heads origin

To compare branches:

  $ git show-branch BRANCH_1 BRANCH_2 [BRANCH_3 ...]

To see a graph of all branches:

  $ git log --graph --oneline --all

To see the branches most recently committed to:

  $ git branch --sort=-committerdate
  $ git for-each-ref --sort=-committerdate refs/heads/

Show latest commits / most recent commits across all branches:

   $ git log --all
   $ git log --all -n 2  # e.g., show most recent two commits

Show latest commit / commit id only / one-line summary of commit:

  $ git log -n1 --pretty=oneline --abbrev-commit

Show what commit introduced a given line of code (\"pickaxe\"):

  $ git log -S\"some line\"

To grab a file from another branch or revision:

  $ git show BRANCH_OR_REV:FULL_PATH_TO_FILE > LOCAL_NAME

To make a repository bare:

  Replace repos with just repos/.git
  Then, inside the now-almost-bare repos, do:
    $ git config --bool core.bare true
  You might also want to rename it so it's clear it's bare:
    $ cd ..
    $ mv repo repo.git

Finding stuff in Git in general -- this is a great article:

  https://medium.com/@tygertec/how-to-find-stuff-in-git-35d4cb8c1845

Git bisect:

  $ git bisect start
  $ git bisect bad SOME_REV
  $ git bisect good SOME_OTHER_REV
  $ git bisect run SOME_SCRIPT
  $ git bisect reset  # done; end bisect and check out master

  All in one:

  $ git bisect start HEAD <good_hash> run SOME_SCRIPT

Pare everything but named files from a repository's history:

  For example, this preserves just 'csv2wiki':

  $ git filter-branch --prune-empty --index-filter                \\
          'git ls-tree -z -r --name-only --full-tree $GIT_COMMIT  \\
          | grep -z -v \"^csv2wiki$\"                             \\
          | xargs -0 -r git rm --cached -r' -- --all

  (See http://stackoverflow.com/questions/5998987/\\
  splitting-a-set-of-files-within-a-git-repo-into-\\
  their-own-repository-preserving for more.)

To cherry-pick commits from one or more divergent repositories:

  ## First, some context:
  ##
  ##   Conservatory/wmctrl -> commits have full 1.00->1.07 release history
  ##   geekless/wmctrl     -> divergent changes made over a 1.07 top-skim
  ##   dancor/wmctrl       -> other divergent changes over a 1.07 top-skim
  ##
  ## To \"merge\" changes from the latter two repositories into the former,
  ## we have to get their master branches into the local repository as
  ## as appropriately-named local branches and then cherry-pick commits.

  $ git clone git@github.com:Conservatory/wmctrl.git
  $ cd wmctrl
  $ git remote add geekless git@github.com:geekless/wmctrl.git
  $ git remote add dancor   git@github.com:dancor/wmctrl.git
  $ git fetch geekless
  From github.com:geekless/wmctrl
   * [new branch]      master     -> geekless/master
  $ git fetch dancor
  From github.com:dancor/wmctrl
   * [new branch]      master     -> dancor/master
  $ git branch -a
  * master
    remotes/dancor/master
    remotes/geekless/master
    remotes/origin/master
  $ git checkout geekless/master
  Note: checking out 'geekless/master'.

  You are in 'detached HEAD' state. You can look around, make experimental
  changes and commit them, and you can discard any commits you make in this
  state without impacting any branches by performing another checkout.

  If you want to create a new branch to retain commits you create, you may
  do so (now or later) by using -b with the checkout command again. Example:

    git checkout -b <new-branch-name>

  HEAD is now at c5c5eb8 Merge pull request #1 from r2rien/master
  $ git branch geekless-master
  $ git checkout master
  Previous HEAD position was c5c5eb8 Merge pull request #1 from r2rien/master
  Switched to branch 'master'
  Your branch is up to date with 'origin/master'.
  $ git log
  [... see commits representing 1.00 -> 1.07 releases ...]
  $ git checkout geekless-master
  Switched to branch 'geekless-master'
  $ git log
  [... see commits representing 1.07 import + divergent changes ...]
  $ git checkout master
  Switched to branch 'master'
  Your branch is up to date with 'origin/master'.
  $ git branch my-merge-branch
  $ git cherry-pick [...etc...]

Check out GitHub pull requests locally (okay, GitHub isn't the same as
Git, but this handily allows one to interact with GitHub without the
proprietary Javascript):

  Add a second \"fetch\" line to the project's .git/config file:

    fetch = +refs/pull/*/head:refs/remotes/origin/pr/*

  Now when you fetch or pull, it'll get all the pr/* branches.
  'git checkout pr/1729' checks out that PR's branch.  To remove the
  ref locally, do 'git update-ref -d refs/remotes/origin/pr/1729'
  (could probably just remove it by hand under .git/refs/ too).

  This came from https://gist.github.com/piscisaureus/3342247; we
  also had https://chat.opentechstrategies.com/#narrow/stream/
  6-Provider-Screening/subject/LEIE/near/50663 in Zulip about it.

To convert a Subversion repository to Git:

  $ git svn clone --no-metadata --authors-file=authors.txt https://svn.red-bean.com/repos/jimb-scripts/trunk/elisp

  Obviously, authors.txt looks like this:

    foo = Foo Random <foo@example.com>
    bar = Bar Quuuux <bar@example.com>

  Add the `--stdlayout' flag if converting from the top of a
  repository that uses the standard TTB layout.

Useful online references:

  \"On undoing, fixing, or removing commits in git\"
  http://sethrobertson.github.io/GitFixUm/fixup.html
")
(kf-gen-displayer kf-git-help
                  (concat
                   "Git is like a BMW: "
                   "a terrific engine surrounded by a cloud of bad decisions.")
                  "*Because command-line arcana == productivity.*")


(defconst kf-latin-abbreviation-help
  "         http://en.wikipedia.org/wiki/List_of_Latin_abbreviations

* A.D.  |  anno Domini  |  \"in the year of the Lord\"

  Used to label or number years in the Julian and Gregorian
  calendars. The AD or the Christian calendar era is based on the
  traditionally reckoned year of the conception or birth of Jesus of
  Nazareth, with AD counting years after the start of this epoch, and
  BC denoting years before the start of the epoch.  Example: The
  United States Civil War began in AD 1861

* a.m.  |  Ante Meridiem  |  \"before midday\"

  Used on the twelve-hour clock to indicate times during the morning.
  Example: We will meet the mayor at 10 a.m. (10:00 in 24hour-clock)

* c., ca., ca or cca.  |  circa  |  \"around\", \"about\", \"approximately\"

  Used in dates to indicate approximately.  Example: The antique clock
  is from c.1900.

* Cap.  |  capitulus  |  \"chapter\"

  Used before a chapter number of laws of the United Kingdom and
  its (former) colonies.  Example: Electronic Transactions Ordinance
  (Cap. 553).'

* cf.  |  confer  |  \"bring together\" and hence \"compare\"

  Confer is the imperative of the Latin verb conferre.  Used
  interchangeably with \"cp.\" in citations indicating the reader should
  compare a statement with that from the cited source.  Example: These
  results were similar to those obtained using different techniques
  (cf. Wilson, 1999 and Ansmann, 1992).

* cp.  |   | compare

  Used interchangeably with \"cf.\" in citations indicating the reader
  should compare a statement with that from the cited source.
  Example: These results were similar to those obtained using
  different techniques (cp. Wilson, 1999 and Ansmann, 1992).

* Cp  |  ceteris paribus  |  \"all other things equal\"

* C.V. or CV | curriculum vitae | \"course of life\"

  A document containing a summary or listing of relevant job
  experience and education. The exact usage of the term varies between
  British English and American English.

* cwt.  |  centum weight  |  \"Hundredweight\"

  cwt. uses a mixture of Latin and English abbreviation.

* D.V.  |  Deo volente  |  \"God willing\"

* DG, D.G. or DEI GRA | Dei gratia | \"by the grace of God\".

  A part of the monarch's title, it is found on all British and
  Canadian coins.

* ead.  |  eadem  |  see id. below.

* et al.  |  et alii | \"and others\", \"and co-workers\".

  It can also stand for et alia, \"and other things\", or et alibi, \"and
  other places\".  Example: These results agree with the ones published
  by Pelon et al. (2002).

* etc.  |  et cetera  |  \"and the others\", \"and other things\", \"and the rest\".

  Other archaic abbreviations include \"&c.\", \"&/c.\", \"&e.\", \"&ct.\",
  and \"&ca.\"  Example: I need to go to the store and buy some pie,
  milk, cheese, etc.

* e.g.  |  exempli gratia  |  \"for example\", \"for instance\".

  Example: The shipping company instituted a surcharge on any items
  weighing over a ton; e.g., a car or truck.

* ff.  |  folio  |  \"and following\"

  This abbreviation is used in citations to indicate an unspecified
  number of following pages following, Example: see page 258ff.

* ibid.  |  ibidem  |  \"in the same place (book, etc.)\"

  The abbreviation is used in citations. It should not be confused
  with the following abbreviation. It is better pronounced ibídem,
  with stress on the second -i- (as it was in Latin).

* id.  |  idem  |  \"the same (man)\".

  It is used to avoid repeating the name of a male author (in
  citations, footnotes, bibliographies, etc.) When quoting a female
  author, use the corresponding feminine form, ead. (eadem), \"the same
  (woman)\" (eadem is pronounced with stress on the first e-).

* i.a.  |  inter alia  |  \"among other things\".

  Example: Ernest Hemingway—author (i.a. 'The Sun Also Rises') and
  friend.

* i.e.  |  id est  |  \"that is\", \"in other words\".

* J.D.  |  Juris Doctor  |  \"teacher of law/rights\".

* lb.  |  libra | \"scales\"

  Used to indicate the pound (mass).

* LL.B.  |  Legum Baccalaureus  |  \"bachelor of laws\"

  The \"LL.\" of the abbreviation for the degree is from the genitive
  plural legum (of lex, legis f., law), thus \"LL.B.\" stands for Legum
  Baccalaureus in Latin. In the United States it was sometimes
  erroneously called \"Bachelor of Legal Letters\" to account for the
  double \"L\" (and therefore sometimes abbreviated as \"L.L.B.\").

* M.A.  |  Magister Artium  |  \"Master of Arts\"

  A postgraduate academic master degree awarded by universities in
  many countries. The degree is typically studied for in fine art,
  humanities, social science or theology and can be either fully
  taught, research-based, or a combination of the two.

* M.O.  |  modus operandi  |  \"method of operating\"

  Sometimes used in criminology to refer to a criminal's method of
  operation.

* N.B.  |  nota bene  |  \"note well\"

  Some people use \"Note\" for the same purpose.  Usually written with
  majuscule (French upper case / 'capital') letters.  Example: N.B.:
  All the measurements have an accuracy of within 5% as they were
  calibrated according to the procedure described by Jackson (1989).

* nem. con.  |  nemine contradicente  |  \"with no one speaking against\"

  The meaning is distinct from \"unanimously\"; \"nem. con.\" simply means
  that nobody voted against. Thus there may have been abstentions from
  the vote.

* op. cit.  |  opere citato  |  \"the work cited\"

  Means in the same article, book or other reference work as was
  mentioned before. It is most often used in citations in a similar
  way to \"ibid\", though \"ibid\" would usually be followed by a page
  number.

* p.a.  |  per annum  |  \"through a year\"

  Is used in the sense of \"yearly\".

* per cent.  |  per centum  |  \"for each one hundred\"

  Commonly \"percent\"

* Ph.D.  |  Philosophiæ Doctor  |  \"Teacher of Philosophy\"

* P.M.  |  Post Meridiem | \"after midday\"

  Used on the twelve-hour clock to indicate times during the
  afternoon.  Example: We will meet the mayor at 2 P.M. (14:00 in
  24hour-clock)

* p.m.a.  |  post mortem auctoris  |  \"after the author's death\"

* p.p. and per pro.  |  per procurationem | \"through the agency of\"

* PRN | pro re nata | \"as needed\"

  Used in prescriptions

* pro tem.  |  pro tempore  |  \"for the time being\", \"temporarily\", \"in place of\"

* P.S.  |  post scriptum  |  \"after what has been written\"

  it is used to indicate additions to a text after the signature of a
  letter.

* Q.D.  |  quaque die  |  \"every day\"

  Used on prescriptions to indicate the medicine should be taken
  daily.

* Q.E.D.  |  quod erat demonstrandum  |  \"which was to be demonstrated\".

  Cited in many texts at the end of a mathematical proof.  Example: At
  the end of the long proof, the professor exclaimed \"Alas, Q.E.D!\"

* q.v.  |  quod videre  |  \"which to see\"

  Used as an imperative.  Used after a term or phrase that should be
  looked up elsewhere in the current document or book. For more than
  one term or phrase, the plural is quae videre (qq.v.).

* Re  |  in re  |  \"in the matter of\", \"concerning\"

  Often used to prefix the subject of traditional letters and
  memoranda. However, when used in an e-mail subject, there is
  evidence that it functions as an abbreviation of \"reply\" rather than
  the word meaning \"in the matter of\". Nominative case singular 'res'
  is the Latin equivalent of 'thing'; singular 're' is the ablative
  case required by 'in'. Some people believe it is short for
  'regarding'.

* REG  |  regina  |  \"queen\"

  A part of the monarch's title, it is found on all British coins
  minted during the reign of a monarch who is a queen. Rex, \"king\"
  (not an abbreviation) is used when the reigning monarch is a king.

* R.I.P.  |  requiescat in pace  |  \"may he/she rest in peace\"

  Used as a short prayer for a dead person, frequently found on
  tombstones. \"R.I.P.\" can also mean requiescant in pace, which is the
  plural form and translates to \"may they rest in peace\" Example:
  R.I.P good grandmother.

* s.o.s.  |  si opus sit  |  \"if there is need\", \"if occasion require\", \"if necessary\"

* stat.  |  statim | \"immediately\"

  Often used in medical contexts.  Example: That patient needs
  attention, stat.!

* viz.  |  videlicet  |  \"namely\", \"to wit\", \"precisely\", \"that is to say\"

  In contradistinction to \"i.e.\" and \"e.g.\", \"viz.\" is used to
  indicate a detailed description of something stated before, and when
  it precedes a list of group members, it implies (near) completeness.
  Example: The noble gases, viz. helium, neon, argon, xenon, krypton
  and radon, show a non-expected behaviour when exposed to this new
  element.

* vs or v.  |  versus  |  \"against\"

  Sometimes is not abbreviated.  Example: The next football game will
  be the Knights vs. the Sea Eagles.
")
(kf-gen-displayer kf-latin-abbreviation-help
                  (concat
                   "No other language is so rich in expressions "
                   "for clarifying what has been previously said.")
                  "*It's what they speak in Latin America.*")


(defconst kf-ssh-help
  "How to change a host key:

  Remove your old host key:
    $ sudo rm -rf /etc/ssh/ssh_host_*

  Generate new key:
    $ sudo ssh-keygen -A
    # (or 'sudo dpkg-reconfigure openssh-server' would work)

  Restart the daemon:
    $ sudo service ssh restart

  Update client-side ~/.ssh/known_hosts files:
    $ ssh-keygen -f \"/home/USERNAME/.ssh/known_hosts\" -R SERVER_IP

  ProxyJump / ProxyCopy:
    $ ssh -J jump.host internal.host
    # end result: you are logged into internal.host
    $ scp -o 'ProxyJump jump.host' foo.txt internal.host:/some/dir
    # end result: foo.txt is in /some/dir/ on internal.host.

  How to get all the SSH fingerprints on a server:
    $ for SSH_KEY_FILE in /etc/ssh/ssh_host_*.pub; do if [ -f ${SSH_KEY_FILE} ]; then ssh-keygen -l -f ${SSH_KEY_FILE}; echo \"\"; fi; done
")
(kf-gen-displayer kf-ssh-help
                   "SSH: I'm hunting wabbits."
                  "*Admit it, you've always wanted to say that.*")


(defconst kf-gnus-help
  "
Incorporate and respool mail from an mbox file:

  In the Group buffer:
  \"G f\" then enter the box file name.
  \"SPACE\" to enter the newly created group.
  \"M P b\" to process-mark all articles in the group's summary
  \"B r\" to respool all the process-marked articles (answer 'nnml' at prompt)

Extract MIME parts, especially inline MIME parts:

  `gnus-summary-save-parts' doesn't seem to work the way I'd
  expect on inline images -- it saves the surrounding HTML but
  not the image itself.  But this worked:

  Put cursor on the MIME part (e.g., on the image).  Then:
  K H --> view in browser (means HTML in /tmp, with image there)
  K o --> in theory, save the MIME part, but does not always work

  https://www.gnu.org/software/emacs/manual/html_node/gnus/MIME-Commands.html
  has more information.

Missing a group:

  If a group exists on disk but not in the Gnus *Group* buffer,
  then do `S s' and type the name of the group.  Although this
  runs `gnus-group-unsubscribe-group', which might seem
  counterintuitive, that's actually what you want: it toggles
  subscription, and somehow you got unsubscribed from that group.

  An alternative method seems to be to do `A A' (to invoke
  `gnus-group-list-active'), which will show all groups and will
  include the new group you're expecting, and if you then enter and
  leave that group... and a bunch of funny stuff happens... then
  when it's all over, you're new group with its new messages will be
  visible in the Groups buffer.  I'm not sure whether this way or the
  `S s' way is better.

Missing mails in some groups:

  If things ever get out of date, like missing articles where the
  article file is present in the directory but somehow doesn't
  show up in the group summary, that's probably because the
  group's .overview file is out-of-whack.  Run
  `nnml-generate-nov-databases' from the top of the mail
  hierarchy; it may take a while, but it'll work.  See

  emacs.stackexchange.com/questions/19358/gnus-doesnt-see-mail-even-though-files-are-there
  www.gnu.org/software/emacs/manual/html_node/gnus/Mail-Spool.html#Mail-Spool

  for details.

Import an mbox file:

  Create a nndoc group based on the mbox file by doing this in the
  Group buffer: `G f /path/to/foo.mbox RET'.

  You now have read-only access to the messages in the mbox.  To
  import them into the regular Gnus groups, enter the new nndoc
  group with `C-u RET' (the C-u is to ensure that all messages
  are retrieved), then mark all the messages with `M P b'.  Once
  they're marked, you can either copy them to another group with
  `B c other.group RET' or respool them (thus running them through
  `nnmail-split-methods') with `B r'.

Marks in the Summary Buffer:

  https://www.gnu.org/software/emacs/manual/html_node/gnus/Summary-Buffer-Lines.html
")
(kf-gen-displayer kf-gnus-help
                 "Gnus: the mailreader that read your mail for you."
                 "*This feature set is larger than my head.*")


(defconst kf-latex-help
  "(http://faculty.cbu.ca/srodney/CompSymbInd.pdf has more.)

Angle brackets: \\textless \\textgreater OR \\textlangle \\textrangle
                (sharper)                 (shallower)

Symbols typically used in running text:

  $   \\$
  %   \\%
  _   \\_
  }   \\}
  &   \\&
  #   \\#
  {   \\{

Keep a block of text all on the same page:

  \\begin{samepage}
  ...
  \\end{samepage}

  Another, more sophisticated way to do it is:

  \\noindent
  \\begin{minipage}{\\textwidth}
  ...
  \\vspace{\\parskip}
  \\end{minipage}

Suppressing page numbering:

  This is a surprisingly complex topic.
  TL;DR: Before \\begin{document}, do

    \\pagenumbering{gobble}

  or maybe

    \\usepackage{nopageno}

  or for one page at a time

    \\thispagestyle{empty}

  or maybe there are other ways, I don't know.

  Why is this so hard?

Ragged right justification:

  Put \\raggedright after \\begin{document}.
  (Except this doesn't seem to actually work, hmmm.)
  Try the 'ragged2e' package and the \\RaggedRight command.

Typeface sizes:

  \\Huge
  \\huge
  \\LARGE
  \\Large
  \\large
  \\normalsize (default)
  \\small
  \\footnotesize
  \\scriptsize
  \\tiny

Tables:

  \\begin{table}[h]
  \\begin{tabular}{|l|l|}
  \\hline
  \\multicolumn{1}{|c|}{\\textbf{Test (50 users)}} & \\multicolumn{1}{|c|}{\\textbf{Scenario}} \\\\ [0.5ex]
  \\hline
  ``Upload Small Text Files (No Poll)'' & \\texttt{login-fileupload-nopoll} \\\\
  \\multicolumn{2}{|l|}{} \\\\
  \\multicolumn{2}{|l|}{\\begin{footnotesize}\\otsurl{...}\\end{footnotesize}} \\\\
  \\hline
  ``Upload Mixed Content Files'' & \\texttt{login-fileupload-poll} \\\\
  \\multicolumn{2}{|l|}{} \\\\
  \\multicolumn{2}{|l|}{\\begin{footnotesize}\\otsurl{...}\\end{footnotesize}} \\\\
  \\hline
  ``Share Uploaded Files'' & \\texttt{login-fileupload-share} \\\\
  \\multicolumn{2}{|l|}{} \\\\
  \\multicolumn{2}{|l|}{\\begin{footnotesize}\\otsurl{...}\\end{footnotesize}} \\\\
  \\hline
  ``Copy Uploaded Files'' & \\texttt{login-fileupload-copy} \\\\
  \\multicolumn{2}{|l|}{} \\\\
  \\multicolumn{2}{|l|}{\\begin{footnotesize}\\otsurl{...}\\end{footnotesize}} \\\\
  \\hline
  ``Move Uploaded Files'' & \\texttt{login-fileupload-move} \\\\
  \\multicolumn{2}{|l|}{} \\\\
  \\multicolumn{2}{|l|}{\\begin{footnotesize}\\otsurl{...}\\end{footnotesize}} \\\\
  \\hline
  ``Baseline (All)'' & \\texttt{baseline} \\\\
  \\multicolumn{2}{|l|}{} \\\\
  \\multicolumn{2}{|l|}{\\begin{footnotesize}\\otsurl{...}\\end{footnotesize}} \\\\
  \\hline
  \\end{tabular}
  \\caption{Blazemeter Test Runs}
  \\label{tab:blazemeter-tests}
  \\end{table}

  (Also, see https://tex.stackexchange.com/questions/12672/\
  which-tabular-packages-do-which-tasks-and-which-packages-conflict
  for a great overview of all the different table packages.)

Dots:

  cdot:           $\\cdot$
  bullet point:   $\\bullet$

Lists:

  \\begin{enumerate}
  \\item ...
  \\end{enumerate}

  \\begin{itemize}
  \\item ...
  \\end{itemize}

    (For itemized lists, you can change the markers at each level with
     \\renewcommand{\\labelitemi}{$\\bullet$}, where \\bullet could be
     \\cdot, \\diamond, -, \\ast, or \\circ, among other things.)

  \\begin{description}
  \\item[Biology] Study of life.
  \\item[Physics] Science of matter and its motion.
  \\item[Psychology] Scientific study of mental processes and behaviour.
  \\end{description}

List styles:

  \\usepackage{enumitem}

  \\begin{itemize}[label=$\\FOO$]
  FOO == \"\\bullet\" | \"-\" | \"\\cdot\" | \"*\" | \"\\star\" | \"\\diamond\"

  \\begin{enumerate}[label=\\BAR]
  BAR == \"arabic*\" | \"roman*\" | \"Roman*\" | \"alph*\" | \"Alph*\"

Arrows:

  In math mode: $\\leftarrow$ | $\\rightarrow$ | $\\uparrow$ | $\\downarrow$

  (Text mode too: \\textrightarrow | \\textleftarrow | \\textuparrow | ...
   But there are more math mode ones, and they look fine, so just use those.)

Commands:

  What's the difference between star and no-star?
  https://tex.stackexchange.com/questions/1050/whats-the-difference-between-newcommand-and-newcommand/1058
  TL;DR: In TeX, def'ing a command means it takes short args not
  paragraphs, and you can use \\long\\def to get around that.  In
  LaTeX, \\newcommand by default is \\long, and then later the star
  form \\newcommand* was added so you could explicitly specify that
  it's only supposed to take short arguments and *not* paragraphs.
  Similarly with \\newenvironment.  Short answer: use the \"*\" unless
  you have a known need to support paragraphs as arguments.

Pre-defined color names that should be available everywhere:

  - yellow
  - white
  - violet
  - teal
  - red
  - purple
  - pink
  - orange
  - olive
  - magenta
  - lime
  - lightgray
  - green
  - gray
  - darkgray
  - cyan
  - brown
  - blue
  - black
")
(kf-gen-displayer kf-latex-help
                 "If you have 100 years to invest, LaTeX is for you."
                 "*LaTeX: The answer to the NSF funding surplus.*")


(defconst kf-debian-help
  "I will never remember this stuff.

To check what version of a package is installed:

  $ dpkg -s PACKAGENAME

To find out what version of Debian I'm running:

  $ lsb_release -a       # or
  $ hostnamectl          # or
  $ cat /etc/os-release
")

(defconst kf-postgres-help
  "It's very important that every database have its own command language.

Basic stuff:

  # su - postgres
  postgres@localhost$ createdb DBNAME -T template_postgis_20
  postgres@localhost$ createuser USERNAME
  postgres@localhost$ psql
  postgres=# \\password               (set password for \"postgres\" superuser)
  Enter new password: ************
  Enter it again: ************
  postgres=# \\q                      (quit; ctrl-d would work as well)
  postgres@localhost$ psql DBNAME     (\"use DBNAME\" w/in psql works too)
  postgres=# drop database DBNAME;
  postgres=# drop user USERNAME;
  postgres=# create database DBNAME;  (same as 'createdb' on cmdline)
  postgres=# create user USERNAME;  (same as 'createuser' on cmdline)
  postgres=# alter user USERNAME with encrypted password 'plaintext';
  postgres=# grant all privileges on database DBNAME to USERNAME;
  postgres=# \\l                      (list all databases; \\list works too)
  postgres=# \\c DBNAME               (connect to db; \\connect works too)
  postgres=# \\d                      (show everything)
  postgres=# \\d  \"TABLENAME\"         (show table schema; quotes are needed)
  postgres=# \\dt \"TABLENAME\"         (show table metadata)
  postgres=# \\pset pager off         (could probably set in ~/.psqlrc)

To connect to a remote database:

  $ psql -h <host> -p <port> -u <database>
  $ psql -h <host> -p <port> -U <username> -W <database>
    Password: <password>
")
(kf-gen-displayer kf-debian-help
                 "Because sysadmin is the new user."
                 "*Debian: An OS for the long term... in every sense.*")


(defconst kf-grep-help
  "To avoid long lines and show just context around match, do this:

  grep -nroP \".{0,20}STRING_TO_MATCH.{0,20}\" FILES_TO_SEARCH_IN

Lose the -n if you don't want line numbers.  For an even better solution:

https://www.topbug.net/blog/2016/08/18/\\
truncate-long-matching-lines-of-grep-a-solution-that-preserves-color/
")
(kf-gen-displayer kf-grep-help
                 "You could figure this out from the man page..."
                 "*If you had a million years.*")


(defconst kf-markdown-help
  "Headers:

  # H1
  ## H2
  ### H3   ...etc...

  Alternates for H1 and H2 are \"=====\" and \"-----\" underlines,
  respectively.

Inline formatting

  (Inline HTML generally works too.)

  Italics:         *single asterisks* or _single underscores_.
  Bold/strong:     **double asterisks** or __double underscores__.
  Combined:        **Yes, you can _do_ that.**
  Strikethrough:   Use ~~two twiddles~~.
  Monospace:       `backticks`

Lists:

  1. First ordered list item
  2. Another item
  ⋅⋅* Unordered sub-list.
  1. Actual numbers don't matter, just that it's a number
  ⋅⋅1. Ordered sub-list
  4. And another item.

  ⋅⋅⋅Properly indented paragraphs within list items.  Notice blank line above & leading spaces (at least 1).

  ⋅⋅⋅To have a line break without a paragraph, you will need to use two trailing spaces.⋅⋅
  ⋅⋅⋅Note that this line is separate, but within the same paragraph.⋅⋅
  ⋅⋅⋅(This is contrary to the typical GFM line break behaviour, where trailing spaces are not required.)

  * Unordered list can use asterisks
  - Or minuses
  + Or pluses

Links:

  [inline style](https://www.example.com)
  [inline with link text](https://www.example.com \"Example.com's Homepage\")
  [relative ref to a repository file](../path/to/something)
  URLs and <URLs> auto-convert: http://www.example.com or <http://www.example.com>

  Reference links are special:

    [reference style][arbitrary case-insensitive ref text]
    [You can use numbers for reference-style link definitions][1]
    Or leave it empty and use the [link text itself].

    Then at the bottom of your doc you can put the ref resolutions:

    [arbitrary case-insensitive reference text]: https://www.mozilla.org
    [1]: http://slashdot.org
    [link text itself]: http://www.reddit.com

Blockquotes:

  > just like you
  > would expect

Images:

  Inline style:

    ![alt text](https://github.com/adam-p/markdown-here/raw/master/src/common/images/icon48.png \"Logo Title Text 1\")

  Reference style:

    ![alt text][logo]

    Then some text, then the image-ref resolution at the bottom
    of the doc:

    [logo]: https://github.com/adam-p/markdown-here/raw/master/src/common/images/icon48.png \"Logo Title Text 2\"

Inline code blocks

  ```javascript
  blah
  ```

  ```python
  blah
  ```

  ```
  No language indicated, so no syntax highlighting.
  ```

  (You can also do it with four spaces of indentation, but it's
  clearer just to use the backticks always.)

Horizontal rule:

  Use three or more hyphens, asterisks, or underscores.

Tables:

  (Not supported everywhere, but GFM and Markdown Here support them.)

    Colons can be used to align columns.

    | Tables        | Are           | Cool  |
    | ------------- |:-------------:| -----:|
    | col 3 is      | right-aligned | $1600 |
    | col 2 is      | centered      |   $12 |
    | zebra stripes | are neat      |    $1 |

  At least 3 dashes separate each header cell.  Outer pipes (|) are
  optional.  Source doesn't need to line up prettily.  Inline
  formatting is supported:

    Markdown | Less | Pretty
    --- | --- | ---
    *Still* | `renders` | **nicely**
    1 | 2 | 3

Sourced liberally from
https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet,
and see also https://www.markdownguide.org/cheat-sheet/ and
of course https://daringfireball.net/projects/markdown/syntax.
")


(defconst kf-mediawiki-help
  "See

  - https://www.mediawiki.org/wiki/Help:Formatting
  - https://www.mediawiki.org/wiki/Help:Links
  - https://www.mediawiki.org/wiki/Help:Images
  - https://www.mediawiki.org/wiki/Help:Tables
  - https://www.mediawiki.org/wiki/Help:Lists

Now, the quick reference.

Formatting:

  ''Italics''

  '''Bold'''

  '''''bold & italic'''''

  <nowiki>no [[wiki]] ''markup''</nowiki>

Links:

  https://www.mediawiki.org/wiki/Help:Links

  [[Some:Internal Page|link text]]

  [https://example.com external link text]

Lists:

  Itemized lists use \"*\"s; enumerated lists use \"#\"s.

  ; item
  : definition
  ; item 2
  : definition 2

  : Single indent
  :: Double indent
  ::::: Multiple indent

  Start a line with a space to get preformatted text;
  other wiki markup is still valid in preformatted text.

Headings and separators:

  == Heading Level 2 ==
  === Heading Level 3 ===
  (etc; do not do Level 1, as that is page name)

  Horizontal rule: use \"----\" on a line by itself.

Images:

  [[File:filename.extension|options|caption]]

  Some options: \"border\", \"frame\", \"thumb\", \"50px\", etc.
  There can be many options, each separated by pipe.
")
(kf-gen-displayer kf-markdown-help
                 "Did the dealer give you good trade-in value for your LaTeX?"
                 "*Markdown: so many different ways to be portable!*")


(defconst kf-org-mode-help
  "Stuff I always have to look up, gathered in one place:

* Todo states / workflow states

  #+TODO: TODO(t) WAIT(w@/!) | DONE(d!) CANCELED(c@)

  !  ==>  timestamp
  @  ==>  note
  /  ==>  stuff after the slash happens after leaving the state
  |  ==>  divider between non-final states and final states

  See also:

  https://orgmode.org/manual/Tracking-TODO-state-changes.html
  https://orgmode.org/manual/Workflow-states.html

* Tags:

  #+TAGS: FISH(f) WILLOW(w)

* Startup stuff

  #+STARTUP: overview | content | showall | showNlevels | showeverything
  (N == 2|3|4|5)

  #+SETUPFILE: ../../blah/blah/blah/foo.org
  #+CATEGORY: SomeNameHere
")
(kf-gen-displayer kf-org-mode-help
                 "I know there's a manual, but I've only got this week."
                 "*Org Mode: Like Lisp, but with asterisks not parentheses.*")


(defun kf-htmlegalize-region (b e)
  "Replace \"&\", \"<\", and \">\" with their HTML escape codes, from B to E.
Is there anything else that should be done to escape HTML?"
  (interactive "r")
  (save-excursion
    (let ((em (copy-marker e)))
      (goto-char b)
      (while (search-forward "&" em t)
        (replace-match "&amp;" nil t))
      (goto-char b)
      (while (search-forward "<" em t)
        (replace-match "&lt;" nil t))
      (goto-char b)
      (while (search-forward ">" em t)
        (replace-match "&gt;" nil t))
      )))

(defun kf-anchor-set (name)
  "Set a standard HTML named anchor at point.
This assumes you are inside the attr area of an HTML element opening tag."
  (interactive "MAnchor name: ")
  (insert (format " id=\"%s\" title=\"%s\"" name name)))

(defun kf-highlight-region (b e)
  "Highlight the region assuming an HTML+CSS \"highlight\" class."
  (interactive "r")
  (save-excursion
    (setq e (copy-marker e))
    (goto-char b)
    (insert "<span class=\"highlight\" >")
    (goto-char e)
    (insert "</span>")))

(defun kf-paragraphize (&optional no-fill)
  "Put paragraph tags around the paragraph at point.
Refill to compensate for the tags, unless prefix arg NO-FILL is non-nil."
  (interactive "P")
  (let* ((markup-flavor (kf-markup-flavor))
         (open-tag
          (cond
           ((eq markup-flavor 'xml) "<para>")
           (t                       "<p>")))
         (close-tag
          (cond
           ((eq markup-flavor 'xml) "</para>")
           (t                       "</p>"))))
    (save-excursion
      (forward-paragraph -1)
      (forward-line 1)
      (forward-word 1)
      (forward-word -1)
      (insert open-tag)
      (forward-paragraph 1)
      (forward-char -1)
      (insert close-tag)
      (unless no-fill (kf-fill-paragraph nil)))))


(defun kf-quoted-printable-decode (b e)
  "Soft-decode the quoted-printable-encoded text from B to E.

Just convert certain common quoted-printable codes to roughly
corresponding ASCII characters suitable for plaintext documents.
(This is different from `quoted-printable-decode-region', which
would actually interpret the quoted-printable text and insert
whatever Unicode characters it specified.)

Example text:

   We won=E2=80=99t be eating the meteorite as proposed.=C2=A0 Instead,
   we (=3D=3D just us, not you) would like to request:

   =E2=80=A2 Yams or nightshades;=C2=A0

   =E2=80=A2 Anything involving beryllium.

   Thank =E2=80=98you=E2=80=99 for your =E2=80=9Ctime=E2=80=9D.
"
  (interactive "r")
  (mapcar (lambda (rplc)
            (let ((qp-code     (car rplc))
                  (replacement (cdr rplc)))
              (goto-char b)
              (replace-string qp-code replacement nil b e)))
          '(
            ("=3D"            . "=")
            ("=E2=80=98"      . "'")
            ("=E2=80=99"      . "'")
            ("=E2=80=9C"      . "\"")
            ("=E2=80=9D"      . "\"")
            ("=C2=A0"         . " ")
            ("=E2=80=A2"      . "*")
            ;; more to come
            ))
  ;; now handle the =\n\\s-* thing
  )


;;; Insertion helpers for characters not in my usual input methods.

(defun kf-checkbox (parg)
  "Insert a checkbox.
With one prefix arg, insert a checked checkbox.
With two prefix args, insert an x'ed checkbox."
  (interactive "P")
  (let ((prefix (car parg)))
    (cond
     ((not prefix)  (insert ?☐)) ; 9744
     ((= prefix 4)  (insert ?☑)) ; 9745
     ((= prefix 16) (insert ?☒)) ; 9746
     (t (error "What do you want me to put in that checkbox?")))))

(defun kf-arrow (type)
  "Insert an arrow of TYPE, where type is a single letter:
    - \"[u]p\"
    - \"[d]own\"
    - \"[l]eft\"
    - \"[r]ight\"
    - \"[h]orizontal double arrow\"
    - \"[v]ertical double arrow\""
  (interactive
   "cArrow type ([u]p, [d]own, [l]eft, [r]ight, [h]oriz, [v]ert): ")
    (insert (cdr (assoc type '((?u . ?↑)
                               (?d . ?↓)
                               (?l . ?←)
                               (?r . ?→)
                               (?h . ?↔)
                               (?v . ?↕)
                               )))))


(defun kf-reverse-lines-region (b e)
  "Reverse the order of lines containing B (inclusive) to E (exclusive)."
  (interactive "r")
  ;; There are two ways to do this: the Emacs way, and the easy way.
  ;; We're going to do it the easy way.
  (save-excursion
    (let ((lines ())
          (b (progn (goto-char b) (beginning-of-line) (point)))
          (e (progn (goto-char e) (beginning-of-line) (point))))
      (goto-char b)
      (while (< (point) e)
        (setq lines
              (cons
               (buffer-substring (point) (progn (forward-line 1) (point)))
               lines)))
      (delete-region b e)
      (mapcar 'insert lines))))

(defun kf-reverse-words (b e)
  "Reverse the order of words in the region from B to E."
  (interactive "*r")
  (apply 'insert
         (reverse (split-string (delete-and-extract-region b e) "\\b"))))

;; From https://www.emacswiki.org/emacs/RandomizeWords
(defun randomize-region (beg end)
  "Randomize the order of words in region."
  (interactive "*r")
  (let ((all (mapcar
              (lambda (w) (if (string-match "\\w" w)
                              ;; Randomize words,
                              (cons (random) w)
                            ;; keep everything else in order.
                            (cons -1 w)))
              (split-string
               (delete-and-extract-region beg end) "\\b")))
        words sorted)
    (mapc (lambda (x)
            ;; Words are numbers >= 0.
            (unless (> 0 (car x))
              (setq words (cons x words))))
          all)
    ;; Random sort!
    (setq sorted (sort words
                       (lambda (a b) (< (car a) (car b)))))
    (mapc
     'insert
     ;; Insert using original list, `all',
     ;; but pull *words* from randomly-sorted list, `sorted'.
     (mapcar (lambda (x)
               (if (> 0 (car x))
                   (cdr x)
                 (prog1 (cdar sorted)
                   (setq sorted (cdr sorted)))))
             all))))


;;; Various kinds of auto-insertion. ;;;;
(defun kf-format-time-string (time-val)
  "Return a date string formatted from TIME-VAL the way I usually want."
  (format-time-string "%Y-%m-%d %H:%M:%S %Z" time-val))

(defun kf-insert-date (&optional thorough)
  "Insert the current date (with day-of-week and time-of-day iff THOROUGH).
If there is only whitespace or nothing between point and the first
column, then prepend asterisk + space and postpend colon + space."
  (interactive "P")
  (let* ((decorate nil)
         (span (buffer-substring-no-properties
                (point) (save-excursion (beginning-of-line) (point)))))
    (save-match-data
      (when (string-match "^\\s-*$" span)
        (setq decorate t)))
    (insert (format-time-string (format "%s%s%%Y-%%m-%%d%s%s"
                                        (if decorate "* " "")
                                        (if thorough "%A, " "")
                                        (if thorough " (%H:%M:%S)" "")
                                        (if decorate ": " ""))))
    (when thorough
      ;; Position cursor on the start of the time portion, since
      ;; that's what's most likely to need editing right now.
      (re-search-backward "([0-9]")
      (forward-char 1))))


;; cpjh help
(defconst my/emacs-help
"EMACS

<ROPT>-…	   			MacOS alt keys
C-x 8 …					Emacs alt keys

C-TAB | C-x ← | →		switch buffer
C-k | C-S-DEL			kill eol | line
C-DEL | C-BS 	 		kill word ← →
C-g						abort action
C-s | C-r				search forward | back
C-z						undo
C-x h					select all text
M-q | M-S-q				fill | un-fill paragraph
M-|						pipe region
M-:						evaluate (interactive)
C-M-;					evaluate region
C-u 0 C-M-\\   			remove indent
C-c d | C-c D			insert date
C-u C-x r N				number lines
C-l						re-centre top
C-M-l					reposition window
M-r						move to top
C-SPC					mark | un-mark
C-x a g					define abbreviation
C-x n n	| C-x n w		narrow | widen buffer
C-x x g					revert buffer (quickly)
C-x x t					toggle truncated lines
C-x C-e					evaluate last standard expression (sexp)
C-x C-q					toggle read-only mode
C-q C-l      			<FF>
C-x [ | C-x ]			<FF> bk | fwd

C-s-SPC					character viewer


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
(kf-gen-displayer my/emacs-help "Display Emacs help." "*Emacs cheatsheet*")

(defconst Henke-Henry
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

(kf-gen-displayer Henke-Henry "Display Henke-Henry Permanent Calendar." "*HHPC*")

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


LIQUID MEASURE CONVERSION

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
+---------+-----------+-----------+-----------+------------+-----------+-----------+")

(kf-gen-displayer measurements "Units of measurement." "*measurements*")

; LocalWords:  un sexp cheatsheet HHPC
