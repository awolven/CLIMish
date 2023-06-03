(in-package :krma.gui)

(defclass application-mixin (krma-application-mixin)
  ())

(defclass application (application-mixin)
  ())

(in-package :hemlock)

(defun krma.gui::post-init-hemlock (hemlock::initial-buffer)
	     (defmode "Echo Area" :major-p t :current-buffer initial-buffer)
	     
	     (setq hi::*echo-area-buffer* (make-instance 'hi:buffer :name "Echo Area" :modes '("Echo Area") :current-buffer initial-buffer))
	     (setq hi::*echo-area-region* (hi::buffer-region hi::*echo-area-buffer*))
	     (setq hi::*echo-area-stream* (hemlock::make-hemlock-character-io-stream hi::*echo-area-buffer*))
	     (setq hi::*echo-area-window* ())
	     
	     (setq hi::*parse-starting-mark* (hi::copy-mark (hi::buffer-point hi::*echo-area-buffer*) :right-inserting))
	     (setq hi::*parse-input-region* (hi::region hi::*parse-starting-mark* (hi::region-end hi::*echo-area-region*)))

	     (defhvar "Current Package"
		 "This variable holds the name of the package currently used for Lisp
   evaluation and compilation.  If it is Nil, the value of *Package* is used
   instead."
	       :value nil
	       :current-buffer initial-buffer)
	     
	     (defmode "Character-Stream"
		 :major-p nil :setup-function 'hemlock::setup-character-stream-mode
		 :current-buffer initial-buffer)
	     
	     (defmode "Interactive-Character-Stream" :major-p nil :setup-function 'hemlock::setup-interactive-character-stream-mode
		      :current-buffer initial-buffer)
	     
	     (defhvar "Message Pause" "The number of seconds to pause after a Message."
	       :value 0.5s0
	       :current-buffer initial-buffer)
	     
	     (defhvar "Beep on Ambiguity"
		 "If non-NIL, beep when completion of a parse is ambiguous."
	       :value t
	       :current-buffer initial-buffer)

	     (defhvar "Ignore File Types"
		 "File types to ignore when trying to complete a filename."
	       :value
	       (list "fasl" "pmaxf" "sparcf" "rtf" "hpf" "axpf" "sgif" "err"
		     "x86f" "lbytef" "core" "trace" ; Lisp
		     "BAK" "CKP"	; Backups & Checkpoints
		     "PS" "ps" "press" "otl" "dvi" "toc" ; Formatting
		     "bbl" "lof" "idx" "lot" "aux"       ; Formatting
		     "mo" "elc"		  ; Other editors
		     "bin" "lbin"	  ; Obvious binary extensions.
		     "o" "a" "aout" "out" ; UNIXY stuff
		     "bm" "onx" "snf"	  ; X stuff
		     "UU" "uu" "arc" "Z" "gz" "tar" ; Binary encoded files
		     )
	       :current-buffer initial-buffer)
	     
	     (hemlock::defattribute "Parse Field Separator"
		 "A value of 1 for this attribute indicates that the corresponding character
  should be considered to be a field separator by the prompting commands.")
	     
	     (setf (hemlock::character-attribute :parse-field-separator #\space) 1)
	     
	     (hemlock::add-hook hemlock::window-buffer-hook
				#'(lambda (window new-buff)
				    (when (and (eq window hi::*echo-area-window*)
					       (not (eq new-buff hi::*echo-area-buffer*)))
				      (hi::editor-error "Can't change echo area window."))))
	     (defhvar "Active Regions Enabled"
		 "When set, some commands that affect the current region only work when the
   region is active."
	       :value t
	       :current-buffer initial-buffer)

	     (defhvar "Highlight Active Region"
		 "When set, the active region will be highlighted on the display if possible."
	       :value t
	       :current-buffer initial-buffer)
	     
	     (hemlock::add-hook hemlock::abort-hook 'hemlock::control-g-deactivate-region)
	     
	     (defhvar "Character Deletion Threshold"
		 "When this many characters are deleted contiguously via KILL-CHARACTERS,
   they are saved on the kill ring -- for example, \"Delete Next Character\",
   \"Delete Previous Character\", or \"Delete Previous Character Expanding
   Tabs\"."
	       :value 5
	       :current-buffer initial-buffer)
	     
	     

	     (hemlock::add-hook hemlock::make-buffer-hook #'(lambda (buffer)
							      (hemlock::hcmd-new-buffer-hook-fun
							       buffer
							       initial-buffer)))
	     
	     (dolist (buff hi::*buffer-list*) (hcmd-new-buffer-hook-fun buff initial-buffer))
	     
	     (defhvar "Scroll Overlap"
		 "The \"Scroll Window\" commands leave this much overlap between screens."
	       :value 2
	       :current-buffer initial-buffer)
  
	     (defhvar "Scroll Redraw Ratio"
		 "This is a ratio of \"inserted\" lines to the size of a window.  When this
   ratio is exceeded, insert/delete line terminal optimization is aborted, and
   every altered line is simply redrawn as efficiently as possible.  For example,
   setting this to 1/4 will cause scrolling commands to redraw the entire window
   instead of moving the bottom two lines of the window to the top (typically
   3/4 of the window is being deleted upward and inserted downward, hence a
   redraw); however, commands line \"New Line\" and \"Open Line\" will still
   efficiently, insert a line moving the rest of the window's text downward."
	       :value 1/4
	       :current-buffer initial-buffer)
	     
	     (defhvar "Universal Argument Default"
		 "Default value for \"Universal Argument\" command."
	       :value 4
	       :current-buffer initial-buffer)
	     
	     (hemlock::add-hook hemlock::window-buffer-hook #'reset-window-display-recentering)

	     (defhvar "String Search Ignore Case"
		 "When set, string searching commands use case insensitive."
	       :value t
	       :current-buffer initial-buffer)

	     (defhvar "Case Replace"
		 "If this is true then \"Query Replace\" will try to preserve case when
  doing replacements."
	       :value t
	       :current-buffer initial-buffer)

	     (defhvar "Pathname Defaults"
		 "This variable contains a pathname which is used to supply defaults
   when we don't have anything better."
	       :value (pathname "")
	       :current-buffer initial-buffer)

	     (defhvar "Last Resort Pathname Defaults"
		 "This variable contains a pathname which is used to supply defaults when
   we don't have anything better, but unlike \"Pathname Defaults\", this is
   never set to some buffer's pathname."
	       :value (pathname "")
	       :current-buffer initial-buffer)
	     
	     (defhvar "Last Resort Pathname Defaults Function"
		 "This variable contains a function that is called when a default pathname is
   needed, the buffer has no pathname, and the buffer's name is not entirely
   composed of alphanumerics.  The default value is a function that simply
   returns \"Last Resort Pathname Defaults\".  The function must take a buffer
   as an argument, and it must return some pathname."
	       :value #'(lambda (buffer)
			  (declare (ignore buffer))
			  (merge-pathnames (hi::value last-resort-pathname-defaults)
					   (hi::value pathname-defaults)))
	       :current-buffer initial-buffer)

	     (defhvar "Revert File Confirm"
		 "If this is true, Revert File will prompt before reverting."
	       :value t
	       :current-buffer initial-buffer)
	     
	     (defhvar "Read File Hook"
		 "These functions are called when a file is read into a buffer.  Each function
   must take two arguments -- the buffer the file was read into and whether the
   file existed (non-nil) or not (nil)."
	       :current-buffer initial-buffer)

	     (defhvar "Add Newline at EOF on Writing File"
		 "This controls whether WRITE-BUFFER-FILE adds a newline at the end of the
   file when it ends at the end of a non-empty line.  When set, this may be
   :ask-user and WRITE-BUFFER-FILE will prompt; otherwise, just add one and
   inform the user.  When nil, never add one and don't ask."
	       :value :ask-user
	       :current-buffer initial-buffer)

	     (defhvar "Keep Backup Files"
		 "When set, .BAK files will be saved upon file writing.  This defaults to nil."
	       :value nil
	       :current-buffer initial-buffer)
	     
	     (defhvar "Write File Hook"
		 "These functions are called when a buffer has been written.  Each function
   must take the buffer as an argument."
	       :current-buffer initial-buffer)
	     
	     (defhvar "Save All Files Confirm"
		 "When non-nil, prompts for confirmation before writing each modified buffer."
	       :value t
	       :current-buffer initial-buffer)

	     (defhvar "Ask for New Buffer"
		 "If true, user is prompted for new buffer after current buffer is
   deleted.  If false then previous buffer is selected automatically."
	       :value nil
	       :current-buffer initial-buffer)
	     
	     (define-file-option "Log" (buffer value)
	       (defhvar "Log File Name"
		   "The name of the file for the change log for the file in this buffer."
		 :buffer buffer  :value value))

	     (defhvar "Log Entry Template"
		 "The format string used to generate the template for a change-log entry.
  Three arguments are given: the file, the date (create if available, now
  otherwise) and the file author, or NIL if not available.  The last \"@\"
  is deleted and the point placed where it was."
	       :value "~A, ~A, Edit by ~:[???~;~:*~:(~A~)~].~%  @~2%"
	       :current-buffer initial-buffer)

	     (defmode "Log"
		 :major-p t
		 :setup-function
		 #'(lambda (buffer)
		     (setf (buffer-minor-mode buffer "Fill" initial-buffer) t))
		 :cleanup-function
		 #'(lambda (buffer)
		     (setf (buffer-minor-mode buffer "Fill" initial-buffer) nil))
		 :current-buffer initial-buffer)

	     (defhvar "Fill Prefix" "The fill prefix in Log mode."
	       :value "  "  :mode "Log"
	       :current-buffer initial-buffer)

	     (hi::define-file-type-hook ("log") (buffer type)
	       (declare (ignore type))
	       (setf (buffer-major-mode buffer initial-buffer) "Log"))

	     (hi::add-hook make-buffer-hook 'add-buffer-history-hook)

	     (hi::add-hook delete-buffer-hook 'delete-buffer-history-hook)

	     (defhvar "Spaces per Tab"
		 "The number of spaces a tab is equivalent to.  NOTE: This is not incorporated
   everywhere in Hemlock yet, so do not change it."
	       :value 8
	       :current-buffer initial-buffer)

	     (defhvar "Indent with Tabs"
		 "Function that takes a mark and a number of spaces and inserts tabs and spaces
   to indent that number of spaces using \"Spaces per Tab\"."
	       :value t
	       :current-buffer initial-buffer)
	     
;;; Note the name of this option has been chosen to be compatible with Emacs.
	     #+NIL
	     (define-file-option "indent-tabs-mode" (buffer value)
	       (let ((value (ignore-errors (let ((*read-eval* nil))
					     (read-from-string value)))))
		 (defhvar "Indent with Tabs" "override by file-option"
		   :buffer buffer
		   :value value)))
	     
	     (defhvar "Indent Function"
		 "Indentation function which is invoked by \"Indent\" command.
   It takes a :left-inserting mark that may be moved."
	       :value #'indent-to-tab-stop
	       :current-buffer initial-buffer)

	     (defattribute "Space"
		 "This attribute is used by the indentation commands to determine which
  characters are treated as space."
	       '(mod 2) 0)

	     (setf (character-attribute :space #\space) 1)
	     (setf (character-attribute :space #\tab) 1)

	     (define-file-type-hook ("pas" "pasmac" "macro" "defs" "spc" "bdy")
                       (buffer type)
	       (declare (ignore type))
	       (setf (buffer-major-mode buffer initial-buffer) "Pascal"))
	     
	     (define-file-type-hook ("lisp" "slisp" "l" "lsp" "mcl") (buffer type)
	       (declare (ignore type))
	       (setf (buffer-major-mode buffer initial-buffer) "Lisp"))
	     
	     (define-file-type-hook ("txt" "text" "tx") (buffer type)
	       (declare (ignore type))
	       (setf (buffer-major-mode buffer initial-buffer) "Text"))

	     (defmode "Grep" :major-p t
		      :documentation
		      "Grep results"
		      :current-buffer initial-buffer)
	     
	     (defmode "Apropos" :major-p t
		      :documentation "Apropos mode presents a list of slave symbols."
		      :current-buffer initial-buffer)
	     
	     (defmode "Typeout" :major-p t
		      :documentation
		      "Typeout is a read-only buffer for pop up displays."
		      :current-buffer initial-buffer)
	     
	     (defhvar "Region Query Size"
		 "A number-of-lines threshold that destructive, undoable region commands
   should ask the user about when the indicated region is too big."
	       :value 30
	       :current-buffer initial-buffer)

	     (defmode "Text" :major-p t
		      :current-buffer initial-buffer)

	     (defmode "CAPS-LOCK" :current-buffer initial-buffer)

	     (defhvar "Source Compare Ignore Extra Newlines"
		 "If T, Source Compare and Source Merge will treat all groups of newlines
  as if they were a single newline.  The default is T."
	       :value t
	       :current-buffer initial-buffer)

	     (defhvar "Source Compare Ignore Case"
		 "If T, Source Compare and Source Merge will treat all letters as if they
  were of the same case.  The default is Nil."
	       :value nil
	       :current-buffer initial-buffer)

	     (defhvar "Source Compare Ignore Indentation"
		 "This determines whether comparisons ignore initial whitespace on a line or
   use the whole line."
	       :value nil
	       :current-buffer initial-buffer)

	     (defhvar "Source Compare Number of Lines"
		 "This variable controls the number of lines Source Compare and Source Merge
  will compare when resyncronizing after a difference has been encountered.
  The default is 3."
	       :value 3
	       :current-buffer initial-buffer)

	     (defhvar "Source Compare Default Destination"
		 "This is a sticky-default buffer name to offer when comparison commands prompt
   for a results buffer."
	       :value "Differences"
	       :current-buffer initial-buffer)

	     (defhvar "Group Find File"
		 "If true, group commands use \"Find File\" to read files, otherwise
  non-resident files are read into the \"Group Search\" buffer."
	       :value nil
	       :current-buffer initial-buffer)
	     
	     (defhvar "Group Save File Confirm"
		 "If true, then the group commands will ask for confirmation before saving
  a modified file."
	       :value t
	       :current-buffer initial-buffer)

	     (defmode "Fill" :current-buffer initial-buffer)

;;;; -- Variables --

	     (defhvar "Fill Column"
		 "Used to determine at what column to force text to the next line."
	       :value 75
	       :current-buffer initial-buffer)

	     (defhvar "Fill Prefix"
		 "String to put before each line when filling."
	       :value ()
	       :current-buffer initial-buffer)

	     (defhvar "Auto Fill Space Indent"
		 "When non-nil, uses \"Indent New Comment Line\" to break lines instead of
   \"New Line\".  However, if there is a fill prefix, it is still preferred."
	       :value nil
	       :current-buffer initial-buffer)
	     
;;;; -- New Attributes --

	     (defattribute "Paragraph Delimiter"
		 "is a character that delimits a paragraph by beginning a line."
	       '(mod 2)
	       0)


;;; (setf (character-attribute :paragraph-delimiter #\@) 1)
;;; (setf (character-attribute :paragraph-delimiter #\\) 1)
;;; (setf (character-attribute :paragraph-delimiter #\/) 1)
;;; (setf (character-attribute :paragraph-delimiter #\-) 1)
;;; (setf (character-attribute :paragraph-delimiter #\') 1)
;;; (setf (character-attribute :paragraph-delimiter #\.) 1)
;;;    These are useful for making certain text formatting command lines
;;; delimit paragraphs.  Anyway, this is what EMACS documentation states,
;;; and #\' and #\. are always paragraph delimiters (don't ask me).

	     (setf (character-attribute :paragraph-delimiter #\space) 1)
	     (setf (character-attribute :paragraph-delimiter #\linefeed) 1)
	     (setf (character-attribute :paragraph-delimiter
					#+CMU #\formfeed #+(or sbcl EXCL CLISP OpenMCL scl) #\page) 1)
	     (setf (character-attribute :paragraph-delimiter #\tab) 1)
	     (setf (character-attribute :paragraph-delimiter #\newline) 1)
	     
	     
	     
	     (defattribute "Sentence Closing Char"
		 "is a delimiting character that may follow a sentence terminator
   such as quotation marks and parentheses."
	       '(mod 2)
	       0)

	     
	     (setf (character-attribute :sentence-closing-char #\") 1)
	     (setf (character-attribute :sentence-closing-char #\') 1)
	     (setf (character-attribute :sentence-closing-char #\)) 1)
	     (setf (character-attribute :sentence-closing-char #\]) 1)
	     (setf (character-attribute :sentence-closing-char #\|) 1)
	     (setf (character-attribute :sentence-closing-char #\>) 1)

	     (defhvar "Paragraph Delimiter Function"
		 "The function that returns whether or not the current line should break the
  paragraph."
	       :value 'default-para-delim-function
	       :current-buffer initial-buffer)
	     
	     (defhvar "Parse Start Function"
		 "Take a mark and move it to the top of a block for paren parsing."
	       :value 'start-of-parse-block
	       :current-buffer initial-buffer)

	     (defhvar "Parse End Function"
		 "Take a mark and move it to the bottom of a block for paren parsing."
	       :value 'end-of-parse-block
	       :current-buffer initial-buffer)

;;;; Parse block finders.

	     (defhvar "Minimum Lines Parsed"
		 "The minimum number of lines before and after the point parsed by Lisp mode."
	       :value 50
	       :current-buffer initial-buffer)
	     
	     (defhvar "Maximum Lines Parsed"
		 "The maximum number of lines before and after the point parsed by Lisp mode."
	       :value 500
	       :current-buffer initial-buffer)
	     
	     (defhvar "Defun Parse Goal"
		 "Lisp mode parses the region obtained by skipping this many defuns forward
   and backward from the point unless this falls outside of the range specified
   by \"Minimum Lines Parsed\" and \"Maximum Lines Parsed\"."
	       :value 2
	       :current-buffer initial-buffer)


	     (defhvar "Lisp Indentation Local Definers"
		 "Forms with syntax like LABELS, MACROLET, etc."
	       :value '("LABELS" "MACROLET" "FLET")
	       :current-buffer initial-buffer)

	     (defhvar "Indent Defanything"
		 "This is the number of special arguments implicitly assumed to be supplied
   in calls to functions whose names begin with \"DEF\".  If set to NIL, this
   feature is disabled."
	       :value 2
	       :current-buffer initial-buffer)

	     (defhvar "Fill Lisp Comment Paragraph Confirm"
		 "This determines whether \"Fill Lisp Comment Paragraph\" will prompt for
   confirmation to fill contiguous lines with the same initial whitespace when
   it is invoked outside of a comment or string."
	       :value t
	       :current-buffer initial-buffer)

	     (defmode "Lisp" :major-p t :setup-function 'setup-lisp-mode
		      :current-buffer initial-buffer)

	     (defhvar "Paren Pause Period"
		 "This is how long commands that deal with \"brackets\" shows the cursor at
   the matching \"bracket\" for this number of seconds."
	       :value 0.5
	       :current-buffer initial-buffer)
	     
	     (defhvar "Paren Pause Period"
		 "This is how long commands that deal with \"brackets\" shows the cursor at
   the matching \"bracket\" for this number of seconds."
	       :value nil
	       :mode "Lisp"
	       :current-buffer initial-buffer)
	     
	     (defhvar "Highlight Open Parens"
		 "When non-nil, causes open parens to be displayed in a different font when
   the cursor is directly to the right of the corresponding close paren."
	       :value t
	       :mode "Lisp"
	       :current-buffer initial-buffer)


	     (defhvar "Open Paren Finder Function"
		 "Should be a function that takes a mark for input and returns either NIL
   if the mark is not after a close paren, or two (temporary) marks
   surrounding the corresponding open paren."
	       :mode "Lisp"
	       :value 'lisp-open-paren-finder-function
	       :current-buffer initial-buffer)

;;;; Some mode variables to coordinate with other stuff.

	     (defhvar "Auto Fill Space Indent"
		 "When non-nil, uses \"Indent New Comment Line\" to break lines instead of
   \"New Line\"."
	       :mode "Lisp" :value t
	       :current-buffer initial-buffer)

	     (defhvar "Comment Start"
		 "String that indicates the start of a comment."
	       :mode "Lisp" :value ";"
	       :current-buffer initial-buffer)

	     (defhvar "Comment Begin"
		 "String that is inserted to begin a comment."
	       :mode "Lisp" :value "; "
	       :current-buffer initial-buffer)

	     (defhvar "Indent Function"
		 "Indentation function which is invoked by \"Indent\" command.
   It must take one argument that is the prefix argument."
	       :value 'indent-for-lisp
	       :mode "Lisp"
	       :current-buffer initial-buffer)

	     (defmode "Editor" :current-buffer initial-buffer)
	     
	     (defhvar "Editor Definition Info"
		 "When this is non-nil, the editor Lisp is used to determine definition
   editing information; otherwise, the slave Lisp is used."
	       :value t
	       :mode "Editor"
	       :current-buffer initial-buffer)

	     (defattribute "Whitespace"
		 "A value of 1 for this attribute indicates that the corresponding character
  should be considered as whitespace.  This is used by the Blank-Line-P
  function.")

	     (setf (character-attribute :whitespace #\space) 1)
	     (setf (character-attribute :whitespace #\linefeed) 1)
	     (setf (character-attribute :whitespace #\tab) 1)
	     (setf (character-attribute :whitespace #\newline) 1)

	     (defattribute "Word Delimiter"
		 "A value of 1 for this attribute indicates that the corresponding character
  separates words.  This is used by the word manipulating commands.")

	     (setf (character-attribute :word-delimiter nil) 1)
	     (setf (character-attribute :word-delimiter #\!) 1)
	     (setf (character-attribute :word-delimiter #\@) 1)
	     (setf (character-attribute :word-delimiter #\#) 1)
	     (setf (character-attribute :word-delimiter #\$) 1)
	     (setf (character-attribute :word-delimiter #\%) 1)
	     (setf (character-attribute :word-delimiter #\^) 1)
	     (setf (character-attribute :word-delimiter #\&) 1)
	     (setf (character-attribute :word-delimiter #\*) 1)
	     (setf (character-attribute :word-delimiter #\() 1)
	     (setf (character-attribute :word-delimiter #\)) 1)
	     (setf (character-attribute :word-delimiter #\-) 1)
	     (setf (character-attribute :word-delimiter #\_) 1)
	     (setf (character-attribute :word-delimiter #\=) 1)
	     (setf (character-attribute :word-delimiter #\+) 1)
	     (setf (character-attribute :word-delimiter #\[) 1)
	     (setf (character-attribute :word-delimiter #\]) 1)
	     (setf (character-attribute :word-delimiter #\\) 1)
	     (setf (character-attribute :word-delimiter #\|) 1)
	     (setf (character-attribute :word-delimiter #\;) 1) 
	     (setf (character-attribute :word-delimiter #\:) 1)
	     (setf (character-attribute :word-delimiter #\') 1)
	     (setf (character-attribute :word-delimiter #\") 1)
	     (setf (character-attribute :word-delimiter #\{) 1)
	     (setf (character-attribute :word-delimiter #\}) 1)
	     (setf (character-attribute :word-delimiter #\,) 1)
	     (setf (character-attribute :word-delimiter #\.) 1)
	     (setf (character-attribute :word-delimiter #\<) 1)
	     (setf (character-attribute :word-delimiter #\>) 1)
	     (setf (character-attribute :word-delimiter #\/) 1)
	     (setf (character-attribute :word-delimiter #\?) 1)
	     (setf (character-attribute :word-delimiter #\`) 1)
	     (setf (character-attribute :word-delimiter #\~) 1)
	     (setf (character-attribute :word-delimiter #\space) 1)
	     (setf (character-attribute :word-delimiter #\linefeed) 1)
	     (setf (character-attribute :word-delimiter
					#+CMU #\formfeed
					#+(or EXCL sbcl CLISP OpenMCL scl) #\page) 1)
	     (setf (character-attribute :word-delimiter #\tab) 1)
	     (setf (character-attribute :word-delimiter #\newline) 1)

	     (shadow-attribute initial-buffer :word-delimiter #\. 0 "Fundamental")
	     (shadow-attribute initial-buffer :word-delimiter #\' 0 "Text")
	     (shadow-attribute initial-buffer :word-delimiter #\backspace 0 "Text")
	     (shadow-attribute initial-buffer :word-delimiter #\_ 0 "Text")
	     
	     (defattribute "Page Delimiter"
		 "This attribute is 1 for characters that separate pages, 0 otherwise.")
	     (setf (character-attribute :page-delimiter nil) 1)
	     (setf (character-attribute :page-delimiter #\page) 1)

	     
	     (defattribute "Lisp Syntax"
		 "These character attribute is used by the lisp mode commands, and possibly
  other people.  The value of ths attribute is always a symbol.  Currently
  defined values are:
   NIL - No interesting properties.
   :space - Acts like whitespace, should not include newline.
   :newline - Newline, man.
   :open-paren - An opening bracket.
   :close-paren - A closing bracket.
   :prefix - A character that is a part of any form it appears before.
   :string-quote - The character that quotes a string.
   :char-quote - The character that escapes a single character.
   :comment - The character that comments out to end of line.
   :constituent - Things that make up symbols."
	       'symbol nil)

	     (setf (character-attribute :lisp-syntax #\space) :space)
	     (setf (character-attribute :lisp-syntax #\tab) :space)

	     (setf (character-attribute :lisp-syntax #\() :open-paren)
	     (setf (character-attribute :lisp-syntax #\)) :close-paren)
	     (setf (character-attribute :lisp-syntax #\') :prefix)
	     (setf (character-attribute :lisp-syntax #\`) :prefix)
	     (setf (character-attribute :lisp-syntax #\#) :prefix)
	     (setf (character-attribute :lisp-syntax #\,) :prefix)
	     (setf (character-attribute :lisp-syntax #\") :string-quote)
	     (setf (character-attribute :lisp-syntax #\\) :char-quote)
	     (setf (character-attribute :lisp-syntax #\;) :comment)
	     (setf (character-attribute :lisp-syntax #\newline) :newline)
	     (setf (character-attribute :lisp-syntax nil) :newline)

	     (do-alpha-chars (ch :both)
	       (setf (character-attribute :lisp-syntax ch) :constituent))

	     (setf (character-attribute :lisp-syntax #\0) :constituent)
	     (setf (character-attribute :lisp-syntax #\1) :constituent)
	     (setf (character-attribute :lisp-syntax #\2) :constituent)
	     (setf (character-attribute :lisp-syntax #\3) :constituent)
	     (setf (character-attribute :lisp-syntax #\4) :constituent)
	     (setf (character-attribute :lisp-syntax #\5) :constituent)
	     (setf (character-attribute :lisp-syntax #\6) :constituent)
	     (setf (character-attribute :lisp-syntax #\7) :constituent)
	     (setf (character-attribute :lisp-syntax #\8) :constituent)
	     (setf (character-attribute :lisp-syntax #\9) :constituent)

	     (setf (character-attribute :lisp-syntax #\!) :constituent)
	     (setf (character-attribute :lisp-syntax #\{) :constituent)
	     (setf (character-attribute :lisp-syntax #\}) :constituent)
	     (setf (character-attribute :lisp-syntax #\[) :constituent)
	     (setf (character-attribute :lisp-syntax #\]) :constituent)
	     (setf (character-attribute :lisp-syntax #\/) :constituent)
	     (setf (character-attribute :lisp-syntax #\@) :constituent)
	     (setf (character-attribute :lisp-syntax #\-) :constituent)
	     (setf (character-attribute :lisp-syntax #\_) :constituent)
	     (setf (character-attribute :lisp-syntax #\+) :constituent)
	     (setf (character-attribute :lisp-syntax #\%) :constituent)
	     (setf (character-attribute :lisp-syntax #\*) :constituent)
	     (setf (character-attribute :lisp-syntax #\$) :constituent)
	     (setf (character-attribute :lisp-syntax #\^) :constituent)
	     (setf (character-attribute :lisp-syntax #\&) :constituent)
	     (setf (character-attribute :lisp-syntax #\~) :constituent)
	     (setf (character-attribute :lisp-syntax #\=) :constituent)
	     (setf (character-attribute :lisp-syntax #\<) :constituent)
	     (setf (character-attribute :lisp-syntax #\>) :constituent)
	     (setf (character-attribute :lisp-syntax #\?) :constituent)
	     (setf (character-attribute :lisp-syntax #\.) :constituent)
	     (setf (character-attribute :lisp-syntax #\:) :constituent)


	     (defattribute "Sentence Terminator"
		 "Used for terminating sentences -- ., !, ?.
   Possibly could make type (mod 3) and use the value of 2 and 1 for spaces
   to place after chacter."
	       '(mod 2)
	       0)

	     (setf (character-attribute :sentence-terminator #\.) 1)
	     (setf (character-attribute :sentence-terminator #\!) 1)
	     (setf (character-attribute :sentence-terminator #\?) 1)

	     (defhvar "Auto Save Pathname Hook"
		 "This hook is called by Auto Save to get a checkpoint pathname when there
   is no pathname associated with a buffer.  If this value is NIL, then
   \"Save\" mode is turned off in the buffer.  Otherwise, the function
   will be called. It should take a buffer as its argument and return either
   NIL or a pathname.  If NIL is returned, then \"Save\" mode is turned off
   in the buffer;  else the pathname returned is used as the checkpoint
   pathname for the buffer."
	       :value #'make-unique-save-pathname
	       :current-buffer initial-buffer)

	     (add-hook write-file-hook 'cleanup-checkpoint)
	     
	     (defmode "Save"
		 :setup-function 'setup-auto-save-mode
		 :cleanup-function 'cleanup-auto-save-mode
		 :current-buffer initial-buffer)

	     (add-hook buffer-modified-hook 'notice-buffer-modified)

	     (add-hook input-hook 'auto-save-count-keys)

;;;; Save Mode Hemlock Variables

;;;
;;; Hemlock variables/parameters for Auto-Save Mode
;;;

	     (defhvar "Auto Save Filename Pattern"
		 "This control-string is used with format to make the filename of the
  checkpoint file.  Format is called with two arguments, the first
  being the directory namestring and the second being the file
  namestring of the default buffer pathname."
	       :value "~A~A.CKP"
	       :current-buffer initial-buffer)

	     (defhvar "Auto Save Key Count Threshold"
		 "This value is the number of destructive/modifying keystrokes that will
  automatically trigger an checkpoint.  This value may be NIL to turn this
  feature off."
	       :value 256
	       :current-buffer initial-buffer)

	     (defhvar "Auto Save Cleanup Checkpoints"
		 "This variable controls whether or not \"Save\" mode will delete the
  checkpoint file for a buffer after it is saved.  If this value is
  non-NIL then cleanup will occur."
	       :value t
	       :current-buffer initial-buffer)

	     (defhvar "Auto Save Checkpoint Frequency"
		 "All modified buffers (in \"Save\" mode) will be checkpointed after this
  amount of time (in seconds).  This value may be NIL (or non-positive)
  to turn this feature off."
	       :value (* 2 60)
	       :hooks '(change-save-frequency)
	       :current-buffer initial-buffer)

	     (defhvar "Auto Save State"
		 "Shadow magic.  This variable is seen when in buffers that are not
  in \"Save\" mode.  Do not change this value or you will lose."
	       :value nil
	       :current-buffer initial-buffer)
;;;; Open parens.

	     (defhvar "Highlight Open Parens"
		 "When non-nil, causes open parens to be displayed in a different font when
   the cursor is directly to the right of the corresponding close paren."
	       :value nil
	       :current-buffer initial-buffer)

	     (defhvar "Open Paren Finder Function"
		 "Should be a function that takes a mark for input and returns either NIL
   if the mark is not after a close paren, or two (temporary) marks
   surrounding the corresponding open paren."
	       :value 'lisp-open-paren-finder-function
	       :current-buffer initial-buffer)

	     (add-hook redisplay-hook 'maybe-highlight-open-parens)

	     (add-hook redisplay-hook 'highlight-active-region)

	     (defmode "Dired" :major-p t
		      :documentation
		      "Dired permits convenient directory browsing and file operations including
   viewing, deleting, copying, renaming, and wildcard specifications."
		      :current-buffer initial-buffer)

	     (defhvar "Dired File Expunge Confirm"
		 "When set (the default), \"Dired Expunge Files\" and \"Dired Quit\" will ask
   for confirmation before deleting the marked files."
	       :value t
	       :current-buffer initial-buffer)

	     (defhvar "Dired Directory Expunge Confirm"
  "When set (the default), \"Dired Expunge Files\" and \"Dired Quit\" will ask
   for confirmation before deleting each marked directory."
	       :value t
	       :current-buffer initial-buffer)
	     ;;;; Dired copying and renaming.

	     (defhvar "Dired Copy File Confirm"
  "Can be either t, nil, or :update.  T means always query before clobbering an
   existing file, nil means don't query before clobbering an existing file, and
   :update means only ask if the existing file is newer than the source."
	       :value t
	       :current-buffer initial-buffer)

	     (defhvar "Dired Rename File Confirm"
  "When non-nil, dired will query before clobbering an existing file."
	       :value t
	       :current-buffer initial-buffer)

	     (defhvar "View Scroll Deleting Buffer"
  "When this is set, \"View Scroll Down\" deletes the buffer when the end
   of the file is visible."
	       :value t
	       :current-buffer initial-buffer)

	     (defmode "View" :major-p nil
		      :setup-function 'setup-view-mode
		      :cleanup-function 'cleanup-view-mode
		      :precedence 5.0
		      :documentation
		      "View mode scrolls forwards and backwards in a file with the buffer read-only.
   Scrolling off the end optionally deletes the buffer."
		      :current-buffer initial-buffer)

	     (defmode "Lisp Listener" :major-p nil :current-buffer initial-buffer)
	     )

(in-package :krma.gui)

(defmethod initialize-instance :after ((app application) &rest initargs)
  (declare (ignore initargs))

  #+NIL
  (setq hemlock::*kill-ring* (hi::make-kill-ring
			      (clui::window-display (krma::main-window app))
			      10))
  #+NIL
  (unless (boundp '*style*)
    (setq *style* (make-instance 'light-style
				 'display (application-display app)
				 'monitor (clui::get-primary-monitor (application-display app)))))
  #+NIL
  (krma.gui::make-echo-area-window :app app)
  #+NIL
  (bt:signal-semaphore (command-loop-go-semaphore (main-window app)))

  (values))


(defun init ()
  (let ((initial-buffer (hi::%init-hemlock)))
    (post-init-hemlock initial-buffer)))

(init)
