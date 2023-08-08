
;;; Threats!
(defconst emacs-threats
  '["because you deserve a brk today."
    "the definitive fritterware."
    "... it's not just a way of life, it's a text editor!"
    "the only text editor known to get indigestion."
    "is that a Lisp interpreter in your editor, or are you just happy to see me?"
    "no job too big... no job."
    "the Swiss Army of Editors."
    "Lovecraft was an optimist."
    "indefensible, reprehensible, and fully extensible."
    "where Turing-completeness is only the beginning..."
    "Resistance is futile; you will be assimilated and byte-compiled."
    "because extension languages should come with the editor built in."
    "if it payed rent for disk space, you'd be rich."
    "a compelling argument for pencil and paper."
    "it's like swatting a fly with a supernova."
    "the only text-editing software to require its own heat sink."
    "featuring the world's first municipal garbage collector!"
    "the road to Hell is paved with extensibility."
    "a learning curve you can use as a plumb line."
    "there's a reason it comes with a built-in psychotherapist."
    "it's not slow --- it's stately."
    "is that a text-editor you've got there, or is it Montana?"
    "more than just a Lisp interpreter, a text editor as well!"
    "freely redistributable; void where prohibited by law."
    "(setq software-quality (/ 1 number-of-authors))"
    "because idle RAM is the Devil's playground."
    "a Lisp interpreter masquerading as ... a Lisp interpreter!"
    "anything free is worth what you paid for it."
    "ballast for RAM."
    "more boundary conditions than the Middle East."
    "you'll understand when you're older, dear."
    "the prosecution rests its case."
    "don't cry -- it won't help."
    "because one operating system isn't enough."
    "well, why *shouldn't* you pay property taxes on your editor?"
    "a real time environment for simulating molasses-based life forms."
    "if SIGINT doesn't work, try a tranquilizer."
    "an inspiring example of form following function... to Hell."
    "because editing your files should be a traumatic experience."
    "or perhaps you'd prefer Russian Roulette, after all?"
    "it's all fun and games, until somebody tries to edit a file."
    "impress your (remaining) friends and neighbors."
    "ed  ::  20-megaton hydrogen bomb : firecracker"
    "because Hell was full."
    "where editing text is like playing Paganini on a glass harmonica."
    "the answer to the world surplus of CPU cycles."
    "don't try this at home, kids."
    "everything *and* the kitchen sink."
    "why choose between a word processor and a Lisp interpreter when you could have neither instead?"]
  "Facts about Emacs that you and your loved ones should be aware of.")

(defconst x-windows-threats
  '["a mistake carried out to perfection."
    "a moment of convenience, a lifetime of regret."
    "a terminal disease."
    "all the problems and twice the bugs."
    "complex nonsolutions to simple nonproblems."
    "dissatisfaction guaranteed."
    "don't get frustrated without it."
    "even your dog won't like it."
    "flaky and built to stay that way."
    "flawed beyond belief."
    "big enough to hurt."
    "foiled again."
    "even not doing anything would have been better than nothing."
    "form follows malfunction."
    "garbage at your fingertips."
    "ignorance is our most important resource."
    "it could be worse, but it'll take time."
    "it could happen to you."
    "let it get in *your* way."
    "live the nightmare."
    "more than enough rope."
    "never had it, never will."
    "no hardware is safe."
    "power tools for power fools."
    "power tools for power losers."
    "putting new limits on productivity."
    "simplicity made complex."
    "some voids are better left unfilled."
    "sometimes you fill a vacuum and it still sucks."
    "the cutting edge of obsolescence."
    "the defacto substandard."
    "the first fully modular software disaster."
    "the joke that kills."
    "the problem for your problem."
    "there's got to be a better way."
    "warn your friends about it."
    "you'd better sit down."
    "you'll envy the dead."
    "graphics hacking :: Roman numerals : sqrt (pi)"]
  "What users said as they collapsed.")

(defconst microsoft-threats
  '["I'm not laughing anymore."
    "where the service packs are larger than the original releases."
    "programs so large they have weather."
    "with our software, there's no limit to what you can't do!"
    "world domination wasn't enough -- we had to write bad software, too!"
    "where even the version numbers aren't Y2K-compliant"
    "making the world a better place... for Microsoft."
    "where `market lock-in' means throwing away the keys."
    "we've got the solution for the problem we sold you."]
  "Read this while rebooting.")

(defconst gdb-threats
  '["the only tool for debugging GDB."
    "oh, I didn't know that \"breakpoint\" meant \"the debugger breaks here\"!"
    ""  ; jimb needs to send me his other two
    ]
  ".")

(defconst food-place-threats
  '["Salad Tank"
    "Fudge Sidewalk"
    "Bun Vault"
    "Venison Breezeway"
    "Burger Ditch"
    "Cake Tent"
    "Fondue Lean-To"
    "Gazpacho Vestibule"
    "Burrito Stairway"
    "Corndog Encampment"
    "Butter Kiln"
    "Kebab Ramp"
    "Tapas Runway"
    "Crumpet Bank"
    "Noodle Cave"
    "Borscht Grotto"
    "Sauerkraut Chimney"
    "Pasta Pergola"
    "Waffle Canyon"
    "Yam Lab"
    "Soda Jail"
    "Curry Wall"
    ]
  "Pizza Hut was just the first tentacle in the building.")

(defun threat-of-the-day (threat-list &optional prefix)
  "Return a morsel of wisdom from the feast in THREAT-LIST.
 Prefix it with PREFIX, if non-nil."
  (or prefix (setq prefix ""))
  (concat prefix (elt threat-list (random (length threat-list)))))

(defmacro threat-of-the-day-generator (category 
                                       doc-string 
                                       &optional no-prefix)
  "Define function `CATEGORY-threat-of-the-day' taking no arguments
  and returning a random threat from the vector `CATEGORY-threats'.
  CATEGORY is a string indicating the threat category.  
  DOC-STRING is the doc string for the generated function.
  Prefix threats with capitalized category unless NO-PREFIX."
  (declare (indent 1))
  `(defun ,(intern (concat category "-threat-of-the-day"))
       ()
       ,doc-string
       (interactive)
       (let ((threat
              (threat-of-the-day 
               (symbol-value (intern (concat ,category "-threats")))
               (unless ,no-prefix (concat (capitalize ,category) ": ")))))
         (if (called-interactively-p)
             (message threat)
           threat))))

(threat-of-the-day-generator "x-windows"
  "Deny it if you dare.")

(threat-of-the-day-generator "emacs"
  "A Cautionary Tale.")

(threat-of-the-day-generator "microsoft" 
  "Straight from public relations.")

(threat-of-the-day-generator "gdb" 
  "*It has a built-in debugger for a reason.")

(threat-of-the-day-generator "food-place" 
  "*The full market space has yet to be explored." t)

(defun kf-wine-review ()
  (interactive)
  ;; Source: subversion/subversion/libsvn_subr/hash.c
  (insert "A forthright entrance, yet coquettish on the tongue, its "
          "deceptively fruity exterior hides the warm mahagony undercurrent "
          "that is the hallmark of Chateau Fraisant-Pître.  Connoisseurs of "
          "the region will be pleased to note the familiar, subtle hints of "
          "mulberries and carburator fluid.  Its confident finish is marred "
          "only by a barely detectable suggestion of rancid squid ink."))

;;; End threats. ;;;
