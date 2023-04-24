;;; ~/.doom.d/winum-config.el -*- lexical-binding: t; -*-



(map! :leader
      "1" #'winum-select-window-1
      "2" #'winum-select-window-2
      "3" #'winum-select-window-3
      "4" #'winum-select-window-4
      "5" #'winum-select-window-5
      "6" #'winum-select-window-6
      "7" #'winum-select-window-7
      "8" #'winum-select-window-8
      "9" #'winum-select-window-9)


(after! which-key
	(push '(("\\(.*\\) 1" . "winum-select-window-1") . ("\\1 1..9" . "window 1..9"))
				which-key-replacement-alist)
	(push '((nil . "winum-select-window-[2-9]") . t) which-key-replacement-alist))
