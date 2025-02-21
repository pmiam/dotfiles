text-mode

(box "┌─" (make-string (length str) ?─) "─┐" n
     "│ " (s str)                       " │" n
     "└─" (make-string (length str) ?─) "─┘" n)
(abox "+-" (make-string (length str) ?-) "-+" n
      "| " (s str)                       " |" n
      "+-" (make-string (length str) ?-) "-+" n)
(cut "--8<---------------cut here---------------start------------->8---" n r n
     "--8<---------------cut here---------------end--------------->8---" n)

text-mode :when git-commit-mode

(feature "[FEAT]")
(enhancement "[ITER]")
(iteration "[ITER]")
(ux "[UX]")
(fix "[FIX]")
(bugfix "[BUGFIX]")
(documents "[DOCS]")
(test "[TEST]")
(chore "[CHORE]")
(cleanup "[CLEAN]")
(deprecate "[DEPRECATE]")
(breaking "[BREAK]")
